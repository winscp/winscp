// FileZilla - a Windows ftp client

// Copyright (C) 2002-2004 - Tim Kosse <tim.kosse@gmx.de>

// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

// CFtpControlSocket.cpp: Implementierungsdatei
//

#include "stdafx.h"
#include "FtpControlSocket.h"
#include "mainthread.h"
#include "transfersocket.h"
#ifndef MPEXT
#include "fileexistsdlg.h"
#endif
#include "pathfunctions.h"
#include "asyncproxysocketlayer.h"
#ifndef MPEXT_NO_SSL
#include "AsyncSslSocketLayer.h"
#endif
#ifndef MPEXT_NO_GSS
#include "AsyncGssSocketLayer.h"
#endif
#include "filezillaapi.h"
#include "misc/utf8.h"
#ifdef MPEXT
#define LENOF(x) ( (sizeof((x))) / (sizeof(*(x))))
#endif

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

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
	CTime ListStartTime;
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
	int nListMode;
	BOOL bPasv;
	CString host;
	UINT port;
	int nFinish;
	t_directory *pDirectoryListing;
	CTime ListStartTime;
	BOOL bTriedPortPasvOnce;
#ifndef MPEXT_NO_ZLIB
	int newZlibLevel;
#endif
	bool lastCmdSentCDUP;
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

CFtpControlSocket::CFtpControlSocket(CMainThread *pMainThread, CFileZillaTools * pTools) : CControlSocket(pMainThread, pTools)
{
	ASSERT(pMainThread);
	m_Operation.nOpMode=0;
	m_Operation.nOpState=-1;
	m_Operation.pData=0;
	m_pTransferSocket=0;
	m_pDataFile=0;
	m_pDirectoryListing=0;
	m_pOwner=pMainThread;
	srand( (unsigned)time( NULL ) );
	m_bKeepAliveActive=FALSE;
	m_bCheckForTimeout=TRUE;
#ifndef MPEXT_NO_SSL
	m_bDidRejectCertificate = FALSE;
#endif

#ifndef MPEXT_NO_ZLIB
	m_useZlib = false;
	m_zlibSupported = false;
	m_zlibLevel = 8;
#endif

	m_bUTF8 = true;
	m_hasClntCmd = false;
#ifdef MPEXT
	m_serverCapabilities.Clear();
	m_ListFile = "";
#endif

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
	LogMessage(__FILE__, __LINE__, this,FZ_LOG_DEBUG, _T("~CFtpControlSocket()"));
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
#ifndef MPEXT_NO_SSL
#define CONNECT_SSL_INIT -8
#define CONNECT_SSL_NEGOTIATE -9
#define CONNECT_SSL_WAITDONE -10
#define CONNECT_SSL_PBSZ -11
#define CONNECT_SSL_PROT -12
#endif
#define CONNECT_FEAT -13
#define CONNECT_SYST -14
#define CONNECT_OPTSUTF8 -15
#define CONNECT_CLNT -16
#define CONNECT_OPTSMLST -17
#define CONNECT_NEEDPASS -18

bool CFtpControlSocket::InitConnect()
{
	USES_CONVERSION;

	// Reset detected capabilities
	m_bAnnouncesUTF8 = false;
	m_hasClntCmd = false;
#ifdef MPEXT
	m_serverCapabilities.Clear();
	m_ListFile = "";
#endif
	m_isFileZilla = false;

	if (m_CurrentServer.nUTF8 == 2)
		m_bUTF8 = false;
	else
		m_bUTF8 = true;

	// Some sanity checks
	if (m_pOwner->IsConnected())
	{
		ShowStatus(_T("Internal error: Connect called while still connected"), 1);
		if (!m_Operation.nOpMode)
			m_Operation.nOpMode = CSMODE_CONNECT;
		DoClose(FZ_REPLY_CRITICALERROR);
		return false;
	}

#ifndef MPEXT_NO_SSL
	if (m_pSslLayer)
	{
		ShowStatus(_T("Internal error: m_pSslLayer not zero in Connect"), 1);
		DoClose(FZ_REPLY_CRITICALERROR);
		return false;
	}
#endif
	if (m_pProxyLayer)
	{
		ShowStatus(_T("Internal error: m_pProxyLayer not zero in Connect"), 1);
		DoClose(FZ_REPLY_CRITICALERROR);
		return false;
	}

#ifndef MPEXT_NO_SSL
	if (m_CurrentServer.nServerType & FZ_SERVERTYPE_LAYER_SSL_IMPLICIT ||
		m_CurrentServer.nServerType & FZ_SERVERTYPE_LAYER_SSL_EXPLICIT ||
		m_CurrentServer.nServerType & FZ_SERVERTYPE_LAYER_TLS_EXPLICIT)
	{
		m_pSslLayer = new CAsyncSslSocketLayer;
		AddLayer(m_pSslLayer);
		TCHAR buffer[1000];
		GetModuleFileName(NULL, buffer, 1000);
		CString filename = buffer;
		int pos = filename.ReverseFind(_MPT('\\'));
		if (pos != -1)
		{
			filename = filename.Left(pos + 1);
			filename += _T("cacert.pem");
			m_pSslLayer->SetCertStorage(filename);
		}
		else
			filename = _MPT("cacert.pem");
	}
#endif

	if (!m_CurrentServer.fwbypass)
	{
		int nProxyType = COptions::GetOptionVal(OPTION_PROXYTYPE);
		if (nProxyType != PROXYTYPE_NOPROXY)
		{
			m_pProxyLayer = new CAsyncProxySocketLayer;
			if (nProxyType == PROXYTYPE_SOCKS4)
				m_pProxyLayer->SetProxy(PROXYTYPE_SOCKS4, T2CA(COptions::GetOption(OPTION_PROXYHOST)), COptions::GetOptionVal(OPTION_PROXYPORT));
			else if (nProxyType==PROXYTYPE_SOCKS4A)
				m_pProxyLayer->SetProxy(PROXYTYPE_SOCKS4A, T2CA(COptions::GetOption(OPTION_PROXYHOST)),COptions::GetOptionVal(OPTION_PROXYPORT));
			else if (nProxyType==PROXYTYPE_SOCKS5)
				if (COptions::GetOptionVal(OPTION_PROXYUSELOGON))
					m_pProxyLayer->SetProxy(PROXYTYPE_SOCKS5, T2CA(COptions::GetOption(OPTION_PROXYHOST)),
											COptions::GetOptionVal(OPTION_PROXYPORT),
											T2CA(COptions::GetOption(OPTION_PROXYUSER)),
											T2CA(CCrypt::decrypt(COptions::GetOption(OPTION_PROXYPASS))));
				else
					m_pProxyLayer->SetProxy(PROXYTYPE_SOCKS5, T2CA(COptions::GetOption(OPTION_PROXYHOST)),
											COptions::GetOptionVal(OPTION_PROXYPORT));
			else if (nProxyType==PROXYTYPE_HTTP11)
				if (COptions::GetOptionVal(OPTION_PROXYUSELOGON))
					m_pProxyLayer->SetProxy(PROXYTYPE_HTTP11, T2CA(COptions::GetOption(OPTION_PROXYHOST)),
											COptions::GetOptionVal(OPTION_PROXYPORT),
											T2CA(COptions::GetOption(OPTION_PROXYUSER)),
											T2CA(CCrypt::decrypt(COptions::GetOption(OPTION_PROXYPASS))));
				else
					m_pProxyLayer->SetProxy(PROXYTYPE_HTTP11, T2CA(COptions::GetOption(OPTION_PROXYHOST)) ,COptions::GetOptionVal(OPTION_PROXYPORT));
			else
				ASSERT(FALSE);
			AddLayer(m_pProxyLayer);
		}
	}

#ifndef MPEXT_NO_GSS
	BOOL bUseGSS = FALSE;
	if (COptions::GetOptionVal(OPTION_USEGSS)
#ifndef MPEXT_NO_SSL
		&& !m_pSslLayer
#endif
	)
	{
		CString GssServers=COptions::GetOption(OPTION_GSSSERVERS);
		LPCSTR lpszAscii=T2CA(m_CurrentServer.host);
		hostent *fullname=gethostbyname(lpszAscii);
		CString host;
		if (fullname)
			host=fullname->h_name;
		else
			host=m_CurrentServer.host;
		host.MakeLower();
		int i;
		while ((i=GssServers.Find(_T(";")))!=-1)
		{
			if ((_MPT(".")+GssServers.Left(i))==host.Right(GssServers.Left(i).GetLength()+1) || GssServers.Left(i)==host)
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
			ShowStatus(_T("Unable to initialize GSS api"), 1);
			DoClose(FZ_REPLY_CRITICALERROR);
			return false;
		}
	}
#endif

	return true;
}

void CFtpControlSocket::Connect(t_server &server)
{
	USES_CONVERSION;

	if (m_Operation.nOpMode)
	{
		ShowStatus(_T("Internal error: m_Operation.nOpMode not zero in Connect"), 1);
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

	m_Operation.nOpState =
#ifndef MPEXT_NO_GSS
	m_pGssLayer ? CONNECT_GSS_INIT :
#endif
	CONNECT_INIT;

#ifndef MPEXT_NO_SSL
	if (server.nServerType & FZ_SERVERTYPE_LAYER_SSL_IMPLICIT)
	{
		if (!m_pSslLayer)
		{
			ShowStatus(_T("Internal error: m_pSslLayer not initialized"), 1);
			DoClose(FZ_REPLY_CRITICALERROR);
			return;
		}
		int res = m_pSslLayer->InitSSLConnection(true, NULL, COptions::GetOptionVal(OPTION_MPEXT_SSLSESSIONREUSE));
#ifndef MPEXT_NO_SSLDLL
		if (res == SSL_FAILURE_LOADDLLS)
			ShowStatus(IDS_ERRORMSG_CANTLOADSSLDLLS, 1);
		else
#endif
		if (res == SSL_FAILURE_INITSSL)
			ShowStatus(IDS_ERRORMSG_CANTINITSSL, 1);
		if (!res)
			PostMessage(m_pOwner->m_hOwnerWnd, m_pOwner->m_nReplyMessageID, FZ_MSG_MAKEMSG(FZ_MSG_SECURESERVER, 1), 0);
		else
		{
			DoClose();
			return;
		}
	}
#endif

	int logontype = COptions::GetOptionVal(OPTION_LOGONTYPE);
	int port;
	CString buf,temp;
	if (server.fwbypass)
		logontype=0;

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
		fwhost=COptions::GetOption(OPTION_FWHOST);
		fwport=COptions::GetOptionVal(OPTION_FWPORT);
		temp=fwhost;
		port=fwport;
		if(fwport!=21)
			fwhost.Format( _T("%s:%d"), fwhost, fwport); // add port to fwhost (only if port is not 21)
	}

	CString hostname = server.host;
	if(server.port!=21)
		hostname.Format( _T("%s:%d"), hostname, server.port); // add port to hostname (only if port is not 21)
	CString str;
	str.Format(IDS_STATUSMSG_CONNECTING, hostname);
	ShowStatus(str, 0);

#ifndef MPEXT_NO_SSL
	if ((server.nServerType & FZ_SERVERTYPE_LAYERMASK) & (FZ_SERVERTYPE_LAYER_SSL_EXPLICIT | FZ_SERVERTYPE_LAYER_TLS_EXPLICIT))
		m_Operation.nOpState = CONNECT_SSL_INIT;
#endif

	if (!CControlSocket::Connect(temp, port))
	{
		if (WSAGetLastError() != WSAEWOULDBLOCK)
		{
			DoClose();
			return;
		}
	}
	m_ServerName = logontype?fwhost:hostname;
	m_LastRecvTime = m_LastSendTime = CTime::GetCurrentTime();

	m_Operation.pData = new CLogonData();
}

void CFtpControlSocket::LogOnToServer(BOOL bSkipReply /*=FALSE*/)
{
	int logontype =
#ifndef MPEXT_NO_GSS
	m_pGssLayer ? 0 :
#endif
	COptions::GetOptionVal(OPTION_LOGONTYPE);
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
	if (m_CurrentServer.fwbypass)
		logontype = 0;

#ifndef MPEXT_NO_SSL
	if (m_Operation.nOpState == CONNECT_SSL_INIT)
	{
		if (m_CurrentServer.nServerType & FZ_SERVERTYPE_LAYER_SSL_EXPLICIT)
		{
			if (!Send("AUTH SSL"))
				return;
		}
		else
		{
			if (!Send("AUTH TLS"))
				return;
		}
		m_Operation.nOpState = CONNECT_SSL_NEGOTIATE;
		return;
	}
	else if (m_Operation.nOpState == CONNECT_SSL_NEGOTIATE)
	{
		int res = GetReplyCode();
		if (res!=2 && res!=3)
		{
			DoClose();
			return;
		}
		else
		{
			if (!m_pSslLayer)
			{
				ShowStatus(_T("Internal error: m_pSslLayer not initialized"), 1);
				DoClose(FZ_REPLY_CRITICALERROR);
				return;
			}
			int res = m_pSslLayer->InitSSLConnection(true, NULL, COptions::GetOptionVal(OPTION_MPEXT_SSLSESSIONREUSE));
#ifndef MPEXT_NO_SSLDLL
			if (res == SSL_FAILURE_LOADDLLS)
				ShowStatus(IDS_ERRORMSG_CANTLOADSSLDLLS, 1);
			else
#endif
			if (res == SSL_FAILURE_INITSSL)
				ShowStatus(IDS_ERRORMSG_CANTINITSSL, 1);
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
		if (!Send(_MPT("PROT P")))
			return;
		m_Operation.nOpState = CONNECT_SSL_PROT;
		return;
	}
	else if (m_Operation.nOpState == CONNECT_SSL_PROT)
	{
		int code = GetReplyCode();
		if (code == 2 || code == 3)
			m_bProtP = true;

		ShowStatus(IDS_STATUSMSG_CONNECTED, 0);
		m_pOwner->SetConnected(TRUE);
		ResetOperation(FZ_REPLY_OK);
		return;
	}
#ifndef MPEXT_NO_GSS
	else
#endif
#endif
#ifndef MPEXT_NO_GSS
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
	else if (m_Operation.nOpState == CONNECT_OPTSMLST)
	{
		int code = GetReplyCode();
		if (code != 2 && code != 3)
			m_serverCapabilities.SetCapability(mlsd_command, no);
		ShowStatus(IDS_STATUSMSG_CONNECTED, 0);
		m_pOwner->SetConnected(TRUE);
		ResetOperation(FZ_REPLY_OK);
		return;
	}
	else if (m_Operation.nOpState == CONNECT_FEAT)
	{
		#ifdef MPEXT
		std::string facts;
		if (m_serverCapabilities.GetCapabilityString(mlsd_command, &facts) == yes)
		{
			ftp_capabilities_t cap = m_serverCapabilities.GetCapabilityString(opts_mlst_command);
			if (cap == unknown)
			{
				std::transform(facts.begin(), facts.end(), facts.begin(), ::tolower);
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
						!strcmp(fact.c_str(), "perm") ||
						!strcmp(fact.c_str(), "unix.mode") ||
						!strcmp(fact.c_str(), "unix.owner") ||
						!strcmp(fact.c_str(), "unix.user") ||
						!strcmp(fact.c_str(), "unix.group") ||
						!strcmp(fact.c_str(), "unix.uid") ||
						!strcmp(fact.c_str(), "unix.gid") ||
						!strcmp(fact.c_str(), "x.hidden"))
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
		PostMessage(m_pOwner->m_hOwnerWnd, m_pOwner->m_nReplyMessageID, FZ_MSG_MAKEMSG(FZ_MSG_CAPABILITIES, 0), (LPARAM)&m_serverCapabilities);
		#endif
		if (!m_bAnnouncesUTF8 && !m_CurrentServer.nUTF8)
			m_bUTF8 = false;
		if (m_bUTF8 && m_hasClntCmd && !m_isFileZilla)
		{
			// Some servers refuse to enable UTF8 if client does not send CLNT command
			// to fix compatibility with Internet Explorer, but in the process breaking
			// compatibility with other clients.
			// Rather than forcing MS to fix Internet Explorer, letting other clients
			// suffer is a questionable decision in my opinion.
			if (Send(_MPT("CLNT FileZilla")))
				m_Operation.nOpState = CONNECT_CLNT;
			return;
		}
		if (m_bUTF8 && !m_isFileZilla)
		{
			// Handle servers that disobey RFC 2640 that have UTF8 in the FEAT
			// response but do not use UTF8 unless OPTS UTF8 ON gets send.
			// However these servers obey a conflicting ietf draft:
			// http://www.ietf.org/proceedings/02nov/I-D/draft-ietf-ftpext-utf-8-option-00.txt
			// servers are, amongst others, G6 FTP Server and RaidenFTPd.
			if (Send(_MPT("OPTS UTF8 ON")))
				m_Operation.nOpState = CONNECT_OPTSUTF8;
			return;
		}

#ifndef MPEXT_NO_SSL
		if ((m_CurrentServer.nServerType & FZ_SERVERTYPE_LAYERMASK) & (FZ_SERVERTYPE_LAYER_SSL_IMPLICIT | FZ_SERVERTYPE_LAYER_SSL_EXPLICIT | FZ_SERVERTYPE_LAYER_TLS_EXPLICIT))
		{
			m_Operation.nOpState = CONNECT_SSL_PBSZ;
			Send(_MPT("PBSZ 0"));
			return;
		}
#endif

		if (m_serverCapabilities.GetCapability(mlsd_command) == yes)
		{
			std::string args;
			if (m_serverCapabilities.GetCapabilityString(opts_mlst_command, &args) == yes &&
				!args.empty())
			{
				m_Operation.nOpState = CONNECT_OPTSMLST;
				Send("OPTS MLST " + CString(args.c_str()));
				return;
			}
		}

		ShowStatus(IDS_STATUSMSG_CONNECTED, 0);
		m_pOwner->SetConnected(TRUE);
		ResetOperation(FZ_REPLY_OK);
		return;
	}
	else if (m_Operation.nOpState == CONNECT_CLNT)
	{
		// See above why we send this command
		if (Send(_MPT("OPTS UTF8 ON")))
			m_Operation.nOpState = CONNECT_OPTSUTF8;
		return;
	}
	else if (m_Operation.nOpState == CONNECT_OPTSUTF8)
	{
#ifndef MPEXT_NO_SSL
		if ((m_CurrentServer.nServerType & FZ_SERVERTYPE_LAYERMASK) & (FZ_SERVERTYPE_LAYER_SSL_IMPLICIT | FZ_SERVERTYPE_LAYER_SSL_EXPLICIT | FZ_SERVERTYPE_LAYER_TLS_EXPLICIT))
		{
			m_Operation.nOpState = CONNECT_SSL_PBSZ;
			Send(_MPT("PBSZ 0"));
			return;
		}
#endif

		ShowStatus(IDS_STATUSMSG_CONNECTED, 0);
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

			if (reply.Find("FileZilla") != -1)
				m_isFileZilla = true;
		}

		if (Send(_MPT("FEAT")))
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
				for (int i = 0; i < m_CurrentServer.pass.GetLength(); i++)
					if (m_CurrentServer.pass.GetAt(i) > 127)
						asciiOnly = false;
				for (int i = 0; i < m_CurrentServer.account.GetLength(); i++)
					if (m_CurrentServer.account.GetAt(i) > 127)
						asciiOnly = false;
				if (!asciiOnly)
				{
					ShowStatus(_T("Login data contains non-ascii characters and server might not be UTF-8 aware. Trying local charset."), 0);
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
		hostname.Format(hostname+  _MPT(":%d"), m_CurrentServer.port); // add port to hostname (only if port is not 21)

	USES_CONVERSION;
#ifndef MPEXT_NO_GSS
	//**** GSS Authentication ****
	if (m_Operation.nOpState==CONNECT_GSS_INIT)  //authenticate
	{
		int	i = m_pGssLayer->GetClientAuth(T2CA(m_CurrentServer.host));
		if (i==-1)
			m_Operation.nOpState = CONNECT_GSS_AUTHDONE;
		else if (i != GSSAPI_AUTHENTICATION_SUCCEEDED)
		{
			m_Operation.nOpState = CONNECT_GSS_FAILED;
			CAsyncRequestData *pData=new CAsyncRequestData;
			pData->nRequestType=FZ_ASYNCREQUEST_GSS_AUTHFAILED;
			pData->nRequestID=m_pOwner->GetNextAsyncRequestID();
			if (!PostMessage(m_pOwner->m_hOwnerWnd, m_pOwner->m_nReplyMessageID, FZ_MSG_MAKEMSG(FZ_MSG_ASYNCREQUEST, FZ_ASYNCREQUEST_GSS_AUTHFAILED), (LPARAM)pData))
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
			//ShowStatus(IDS_STATUSMSG_GSSAUTH, 0);
			PostMessage(m_pOwner->m_hOwnerWnd,m_pOwner->m_nReplyMessageID, FZ_MSG_MAKEMSG(FZ_MSG_SECURESERVER, TRUE), 0);
			if (Send(_MPT("CWD .")))
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
			if (!PostMessage(m_pOwner->m_hOwnerWnd, m_pOwner->m_nReplyMessageID, FZ_MSG_MAKEMSG(FZ_MSG_ASYNCREQUEST, FZ_ASYNCREQUEST_GSS_AUTHFAILED), (LPARAM)pData))
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
			//ShowStatus(IDS_STATUSMSG_GSSAUTH, 0);
			PostMessage(m_pOwner->m_hOwnerWnd,m_pOwner->m_nReplyMessageID, FZ_MSG_MAKEMSG(FZ_MSG_SECURESERVER, TRUE), 0);
			if (Send(_MPT("CWD .")))
				m_Operation.nOpState = CONNECT_GSS_CWD;
			return;
		}
	}
	else if (m_Operation.nOpState == CONNECT_GSS_CWD)
	{ // authentication succeeded, we're now get the response to the CWD command
		if (GetReplyCode() == 2) // we're logged on
		{
			if (Send(_MPT("SYST")))
				m_Operation.nOpState = CONNECT_SYST;
			return;
		}
		else
			//GSS authentication complete but we still have to go through the standard logon procedure
			m_Operation.nOpState = CONNECT_INIT;
	}
#endif

	if (m_Operation.nOpState==CONNECT_INIT)
	{
		if (logontype)
		{

			CString str;
			str.Format(IDS_STATUSMSG_FWCONNECT,hostname);
			ShowStatus(str,0);
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
			if (Send(_MPT("SYST")))
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
			(m_CurrentServer.user == _MPT("anonymous") || m_CurrentServer.user == _MPT("")))
		{
			//Extract user from kerberos ticket
			char str[256];
			if (m_pGssLayer->GetUserFromKrbTicket(str))
				m_CurrentServer.user = str;
			if (m_CurrentServer.user == _MPT(""))
			{
				CGssNeedUserRequestData *pData = new CGssNeedUserRequestData;
				pData->nRequestID = m_pOwner->GetNextAsyncRequestID();
				pData->nOldOpState = m_Operation.nOpState;
				m_Operation.nOpState = CONNECT_GSS_NEEDUSER;
				if (!PostMessage(m_pOwner->m_hOwnerWnd, m_pOwner->m_nReplyMessageID, FZ_MSG_MAKEMSG(FZ_MSG_ASYNCREQUEST, FZ_ASYNCREQUEST_GSS_NEEDUSER), (LPARAM)pData))
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
		else if ((i == 1 || i == 11) && (m_CurrentServer.pass == COptions::GetOption(OPTION_ANONPWD) || m_CurrentServer.pass == ""))
		{
			CGssNeedPassRequestData *pData=new CGssNeedPassRequestData;
			pData->nRequestID=m_pOwner->GetNextAsyncRequestID();
			pData->nOldOpState = m_Operation.nOpState;
			m_Operation.nOpState = CONNECT_GSS_NEEDPASS;
			if (!PostMessage(m_pOwner->m_hOwnerWnd, m_pOwner->m_nReplyMessageID, FZ_MSG_MAKEMSG(FZ_MSG_ASYNCREQUEST, FZ_ASYNCREQUEST_GSS_NEEDPASS), (LPARAM)pData))
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
	bool needpass = (m_CurrentServer.pass == COptions::GetOption(OPTION_ANONPWD)) || (m_CurrentServer.pass == _MPT(""));
	switch(logonseq[logontype][m_Operation.nOpState])
	{
		case 0:
			temp=_MPT("USER ")+m_CurrentServer.user;
			break;
		case 1:
			if (needpass && !pData->waitForAsyncRequest && !pData->gotPassword)
			{
				CNeedPassRequestData *pNeedPassRequestData = new CNeedPassRequestData();
				pNeedPassRequestData->nRequestID = m_pOwner->GetNextAsyncRequestID();
				pNeedPassRequestData->nOldOpState = m_Operation.nOpState;
				m_Operation.nOpState = CONNECT_NEEDPASS;
				if (!PostMessage(m_pOwner->m_hOwnerWnd, m_pOwner->m_nReplyMessageID, FZ_MSG_MAKEMSG(FZ_MSG_ASYNCREQUEST, FZ_ASYNCREQUEST_NEEDPASS), (LPARAM)pNeedPassRequestData))
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
				temp=_MPT("PASS ")+m_CurrentServer.pass;
			}
			else
			{
				return;
			}
			break;
		case 2:
			temp=_MPT("ACCT ")+CCrypt::decrypt(COptions::GetOption(OPTION_FWPASS));
			break;
		case 3:
			temp=_MPT("USER ")+COptions::GetOption(OPTION_FWUSER);
			break;
		case 4:
			temp=_MPT("PASS ")+CCrypt::decrypt(COptions::GetOption(OPTION_FWPASS));
			break;
		case 5:
			temp=_MPT("SITE ")+hostname;
			break;
		case 6:
			temp=_MPT("USER ")+m_CurrentServer.user+_MPT("@")+hostname;
			break;
		case 7:
			temp=_MPT("OPEN ")+hostname;
			break;
		case 8:
			temp=_MPT("USER ")+COptions::GetOption(OPTION_FWUSER)+_MPT("@")+hostname;
			break;
		case 9:
			temp=_MPT("USER ")+m_CurrentServer.user+_MPT("@")+hostname+_MPT(" ")+COptions::GetOption(OPTION_FWUSER);
			break;
		case 10:
			temp=_MPT("USER ")+m_CurrentServer.user+_MPT("@")+COptions::GetOption(OPTION_FWUSER)+_MPT("@")+hostname;
			break;
		case 11:
			if (needpass && !pData->waitForAsyncRequest && !pData->gotPassword)
			{
				CNeedPassRequestData *pNeedPassRequestData = new CNeedPassRequestData();
				pNeedPassRequestData->nRequestID = m_pOwner->GetNextAsyncRequestID();
				pNeedPassRequestData->nOldOpState = m_Operation.nOpState;
				m_Operation.nOpState = CONNECT_NEEDPASS;
				if (!PostMessage(m_pOwner->m_hOwnerWnd, m_pOwner->m_nReplyMessageID, FZ_MSG_MAKEMSG(FZ_MSG_ASYNCREQUEST, FZ_ASYNCREQUEST_NEEDPASS), (LPARAM)pNeedPassRequestData))
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
				temp=_MPT("PASS ")+m_CurrentServer.pass+_MPT("@")+CCrypt::decrypt(COptions::GetOption(OPTION_FWPASS));
			}
			else
			{
				return;
			}
			break;
		case 12:
			if (m_CurrentServer.account == _T(""))
				temp = _T("ACCT default");
			else
				temp = _T("ACCT ") + m_CurrentServer.account;
			break;
	}
	// send command, get response
	if(!Send(temp))
		return;
}

#define BUFFERSIZE 4096
void CFtpControlSocket::OnReceive(int nErrorCode)
{
	LogMessage(__FILE__, __LINE__, this,FZ_LOG_DEBUG, _T("OnReceive(%d)  OpMode=%d OpState=%d"), nErrorCode, m_Operation.nOpMode, m_Operation.nOpState);

	m_LastRecvTime = CTime::GetCurrentTime();
	PostMessage(m_pOwner->m_hOwnerWnd, m_pOwner->m_nReplyMessageID, FZ_MSG_MAKEMSG(FZ_MSG_SOCKETSTATUS, FZ_SOCKETSTATUS_RECV), 0);

	if (!m_pOwner->IsConnected())
	{
		if (!m_Operation.nOpMode)
		{
			LogMessage(__FILE__, __LINE__, this, FZ_LOG_INFO, _T("Socket has been closed, don't process receive") );
			return;
		}
		m_MultiLine = "";
		CString str;
		str.Format(IDS_STATUSMSG_CONNECTEDWITH, m_ServerName);
		ShowStatus(str, 0);
		m_pOwner->SetConnected(TRUE);
	}
	char *buffer = new char[BUFFERSIZE];
	int numread = Receive(buffer, BUFFERSIZE);

	if (numread == SOCKET_ERROR)
	{
		delete [] buffer;
		buffer = NULL;
		if (GetLastError() != WSAEWOULDBLOCK)
		{
			ShowStatus(IDS_STATUSMSG_DISCONNECTED, 1);
			DoClose();
		}
		return;
	}
	if (!numread)
	{
		delete [] buffer;
		buffer = NULL;
		ShowStatus(IDS_STATUSMSG_DISCONNECTED, 1);
		DoClose();
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
					if (!utf8_valid((const unsigned char*)utf8, strlen(utf8)))
					{
						if (m_CurrentServer.nUTF8 != 1)
						{
							LogMessage(__FILE__, __LINE__, this, FZ_LOG_WARNING, _T("Server does not send proper UTF-8, falling back to local charset"));
							m_bUTF8 = false;
						}
						ShowStatus(A2CT(utf8), 3);
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
							ShowStatus(W2CT(p1), 3);
							delete [] p1;
						}
					}
				}
				else
					ShowStatus(A2CT(m_RecvBuffer.back()), 3);
				//Check for multi-line responses
				if (m_RecvBuffer.back().GetLength() > 3)
				{
					if (m_MultiLine != "")
					{
						if (m_RecvBuffer.back().Left(4) != m_MultiLine)
						{
							DiscardLine(m_RecvBuffer.back());
 							m_RecvBuffer.pop_back();
						}
						else // end of multi-line found
						{
							m_MultiLine = "";
							m_pOwner->PostThreadMessage(m_pOwner->m_nInternalMessageID, FZAPI_THREADMSG_PROCESSREPLY, 0);
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
						m_pOwner->PostThreadMessage(m_pOwner->m_nInternalMessageID, FZAPI_THREADMSG_PROCESSREPLY, 0);
				}
				else
					m_RecvBuffer.pop_back();
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
	if ( reply == _T("") )
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
		ListFile();
	else if (m_Operation.nOpMode&CSMODE_DELETE)
		Delete( _T(""),CServerPath());
	else if (m_Operation.nOpMode&CSMODE_RMDIR)
		RemoveDir( _T(""),CServerPath());
	else if (m_Operation.nOpMode&CSMODE_MKDIR)
		MakeDir(CServerPath());
	else if (m_Operation.nOpMode&CSMODE_RENAME)
		Rename(_T(""), _T(""), CServerPath(), CServerPath());

	if (!m_RecvBuffer.empty())
		m_RecvBuffer.pop_front();
}

void CFtpControlSocket::OnConnect(int nErrorCode)
{
	LogMessage(__FILE__, __LINE__, this,FZ_LOG_DEBUG, _T("OnConnect(%d)  OpMode=%d OpState=%d"), nErrorCode, m_Operation.nOpMode, m_Operation.nOpState);

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
#ifndef MPEXT_NO_SSL
				m_pSslLayer ? IDS_STATUSMSG_CONNECTEDWITHSSL :
#endif
				IDS_STATUSMSG_CONNECTEDWITH, m_ServerName);
			ShowStatus(str,0);
		}
	}
	else
	{
		if (nErrorCode == WSAHOST_NOT_FOUND)
			ShowStatus(IDS_ERRORMSG_CANTRESOLVEHOST, 1);
#ifdef MPEXT
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
			ShowStatus(CString(Buffer, Len), 1);
		}
#endif
		DoClose();
	}
}

BOOL CFtpControlSocket::Send(CString str)
{
	USES_CONVERSION;

	ShowStatus(str, 2);
	str += _MPT("\r\n");
	int res = 0;
	if (m_bUTF8)
	{
		LPCWSTR unicode = T2CW(str);
		int len = WideCharToMultiByte(CP_UTF8, 0, unicode, -1, 0, 0, 0, 0);
		if (!len)
		{
			ShowStatus(IDS_ERRORMSG_CANTSENDCOMMAND, 1);
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
			ShowStatus(IDS_ERRORMSG_CANTSENDCOMMAND, 1);
			DoClose();
			return FALSE;
		}
		if (res != sendLen)
		{
			if (res == -2)
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
			ShowStatus(IDS_ERRORMSG_CANTSENDCOMMAND, 1);
			DoClose();
			return FALSE;
		}
		if (res != sendLen)
		{
			if (res == -2)
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
		// otherwise we may happen to timeout immediatelly after sending request if
		// CheckForTimeout occurs in between and we haven't received any data for a while
		m_LastRecvTime = m_LastSendTime;
		PostMessage(m_pOwner->m_hOwnerWnd, m_pOwner->m_nReplyMessageID, FZ_MSG_MAKEMSG(FZ_MSG_SOCKETSTATUS, FZ_SOCKETSTATUS_SEND), 0);
	}
	return TRUE;
}

int CFtpControlSocket::GetReplyCode()
{
	if (m_RecvBuffer.empty())
		return 0;
	CStringA str = m_RecvBuffer.front();
	if (str == "")
		return 0;
	else
		return str[0]-'0';
}

void CFtpControlSocket::DoClose(int nError /*=0*/)
{
	LogMessage(__FILE__, __LINE__, this,FZ_LOG_DEBUG, _T("DoClose(%d)  OpMode=%d OpState=%d"), nError, m_Operation.nOpMode, m_Operation.nOpState);

	m_bCheckForTimeout=TRUE;
	m_pOwner->SetConnected(FALSE);
	m_bKeepAliveActive=FALSE;
	PostMessage(m_pOwner->m_hOwnerWnd, m_pOwner->m_nReplyMessageID, FZ_MSG_MAKEMSG(FZ_MSG_SECURESERVER, FALSE), 0);
	if (nError & FZ_REPLY_CRITICALERROR)
		nError |= FZ_REPLY_ERROR;
	ResetOperation(FZ_REPLY_ERROR|FZ_REPLY_DISCONNECTED|nError);
	m_RecvBuffer.clear();
	m_MultiLine = "";
#ifndef MPEXT_NO_SSL
	m_bDidRejectCertificate = FALSE;
#endif

#ifndef MPEXT_NO_ZLIB
	m_useZlib = false;
	m_zlibSupported = false;
	m_zlibLevel = 0;
#endif

	m_bUTF8 = false;
	m_hasClntCmd = false;
#ifdef MPEXT
	m_serverCapabilities.Clear();
	m_ListFile = "";
#endif

	m_awaitsReply = false;
	m_skipReply = false;

	delete [] m_sendBuffer;
	m_sendBuffer = 0;
	m_sendBufferLen = 0;

	m_bProtP = false;

	m_mayBeMvsFilesystem = false;
	m_mayBeBS2000Filesystem = false;
 
	CControlSocket::Close();
}

void CFtpControlSocket::Disconnect()
{
	ASSERT(!m_Operation.nOpMode);
	m_Operation.nOpMode=CSMODE_DISCONNECT;
	DoClose();
	ShowStatus(IDS_STATUSMSG_DISCONNECTED,0); //Send the disconnected message to the message log
}

void CFtpControlSocket::CheckForTimeout()
{
	if (!m_Operation.nOpMode && !m_bKeepAliveActive)
		return;
	if (!m_bCheckForTimeout)
		return;
	int delay=COptions::GetOptionVal(OPTION_TIMEOUTLENGTH);
	if (m_pTransferSocket)
	{
		int res=m_pTransferSocket->CheckForTimeout(delay);
		if (res)
			return;
	}
	CTimeSpan span=CTime::GetCurrentTime()-m_LastRecvTime;
	if (span.GetTotalSeconds()>=delay)
	{
		ShowStatus(IDS_ERRORMSG_TIMEOUT, 1);
		DoClose();
	}
}

void CFtpControlSocket::FtpCommand(LPCTSTR pCommand)
{
	LogMessage(__FILE__, __LINE__, this,FZ_LOG_DEBUG, _T("FtpCommand(%s)  OpMode=%d OpState=%d"), pCommand, m_Operation.nOpMode, m_Operation.nOpState);
	m_Operation.nOpMode=CSMODE_COMMAND;
	Send(pCommand);
}

CString CFtpControlSocket::GetListingCmd()
{
	CString cmd;
	// 0 = on, 1 = off, 2 = auto
	if ((m_CurrentServer.iUseMlsd == 0) ||
		((m_CurrentServer.iUseMlsd != 1) &&
		 (m_serverCapabilities.GetCapability(mlsd_command) == yes)))
	{
		cmd = _T("MLSD");
	}
	else
	{
		cmd = _T("LIST");
		if (COptions::GetOptionVal(OPTION_MPEXT_SHOWHIDDEN) && !(m_CurrentServer.nServerType & (FZ_SERVERTYPE_SUB_FTP_MVS | FZ_SERVERTYPE_SUB_FTP_VMS | FZ_SERVERTYPE_SUB_FTP_BS2000)))
			cmd += _T(" -a");
	}
	return cmd;
}

void CFtpControlSocket::List(BOOL bFinish, int nError /*=FALSE*/, CServerPath path /*=CServerPath()*/, CString subdir /*=_MPT("")*/,int nListMode/*=0*/)
{
	LogMessage(__FILE__, __LINE__, this,FZ_LOG_DEBUG, _T("List(%s,%d,\"%s\",\"%s\",%d)  OpMode=%d OpState=%d"), bFinish?_T("TRUE"):_T("FALSE"), nError, path.GetPath(), subdir, nListMode,
				m_Operation.nOpMode, m_Operation.nOpState);

	USES_CONVERSION;

	#define LIST_INIT	-1
	#define LIST_PWD	0
	#define LIST_CWD	1
	#define LIST_PWD2	2
	#define LIST_CWD2	3
	#define LIST_PWD3	4
	#define LIST_MODE	5
	#define LIST_OPTS	6
	#define LIST_PORT_PASV	7
	#define LIST_TYPE	8
	#define LIST_LIST	9
	#define LIST_WAITFINISH	10

	ASSERT(!m_Operation.nOpMode || m_Operation.nOpMode&CSMODE_LIST);

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
		if (COptions::GetOptionVal(OPTION_DEBUGSHOWLISTING))
			m_pTransferSocket->m_pListResult->SendToMessageLog(m_pOwner->m_hOwnerWnd, m_pOwner->m_nReplyMessageID);
		pData->pDirectoryListing->direntry = m_pTransferSocket->m_pListResult->getList(num, pData->ListStartTime);
		pData->pDirectoryListing->num = num;
		if (m_pTransferSocket->m_pListResult->m_server.nServerType & FZ_SERVERTYPE_SUB_FTP_VMS && m_CurrentServer.nServerType & FZ_SERVERTYPE_FTP)
			m_CurrentServer.nServerType |= FZ_SERVERTYPE_SUB_FTP_VMS;

		pData->pDirectoryListing->server = m_CurrentServer;
		pData->pDirectoryListing->path.SetServer(pData->pDirectoryListing->server);
		if (pData->rawpwd != _MPT(""))
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
			ShowStatus(IDS_STATUSMSG_DIRLISTSUCCESSFUL,0);
#ifndef MPEXT_NO_CACHE
			CDirectoryCache cache;
			cache.Lock();
			t_directory dir;
			if (!pData->path.IsEmpty() && pData->subdir!=_MPT(""))
			{
				if (cache.Lookup(pData->pDirectoryListing->path, pData->pDirectoryListing->server, dir))
					pData->pDirectoryListing->Merge(dir, pData->ListStartTime);
				cache.Store(*pData->pDirectoryListing,pData->path,pData->subdir);
			}
			else
			{
				if (cache.Lookup(pData->pDirectoryListing->path, pData->pDirectoryListing->server, dir))
					pData->pDirectoryListing->Merge(dir, pData->ListStartTime);
				cache.Store(*pData->pDirectoryListing);
			}
			cache.Unlock();
#endif
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
				pData->rawpwd[0] != _MPT('/'))
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
#ifndef MPEXT_NO_CACHE
				if (pData->nListMode & FZ_LIST_USECACHE)
				{
					t_directory dir;
					CDirectoryCache cache;
					BOOL res = cache.Lookup(m_pOwner->GetCurrentPath(), m_CurrentServer, dir);
					if (res)
					{
						BOOL bExact = TRUE;
						if (pData->nListMode & FZ_LIST_EXACT)
							for (int i=0; i<dir.num; i++)
								if (dir.direntry[i].bUnsure || (dir.direntry[i].size==-1 && !dir.direntry[i].dir))
								{
									bExact = FALSE;
									break;
								}
						if (bExact)
						{
							ShowStatus(IDS_STATUSMSG_DIRLISTSUCCESSFUL, 0);
							SetDirectoryListing(&dir);
							ResetOperation(FZ_REPLY_OK);
							return;
						}
					}
				}
#endif
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
			if (pData->subdir != _MPT(""))
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
#ifndef MPEXT_NO_CACHE
				if (pData->nListMode & FZ_LIST_USECACHE)
				{
					t_directory dir;
					CDirectoryCache cache;
					BOOL res = cache.Lookup(m_pOwner->GetCurrentPath(), m_CurrentServer, dir);
					if (res)
					{
						BOOL bExact = TRUE;
						if (pData->nListMode & FZ_LIST_EXACT)
							for (int i=0; i<dir.num; i++)
								if (dir.direntry[i].bUnsure || (dir.direntry[i].size==-1 && !dir.direntry[i].dir))
								{
									bExact = FALSE;
									break;
								}
						if (bExact)
						{
							ShowStatus(IDS_STATUSMSG_DIRLISTSUCCESSFUL, 0);
							SetDirectoryListing(&dir);
							ResetOperation(FZ_REPLY_OK);
							return;
						}
					}
				}
#endif
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
#ifndef MPEXT_NO_CACHE
				if (pData->nListMode & FZ_LIST_USECACHE)
				{
					t_directory dir;
					CDirectoryCache cache;
					BOOL res = cache.Lookup(m_pOwner->GetCurrentPath(), m_CurrentServer, dir);
					if (res)
					{
						BOOL bExact = TRUE;
						if (pData->nListMode & FZ_LIST_EXACT)
							for (int i=0; i<dir.num; i++)
								if (dir.direntry[i].bUnsure || (dir.direntry[i].size==-1 && !dir.direntry[i].dir))
								{
									bExact = FALSE;
									break;
								}
						if (bExact)
						{
							ShowStatus(IDS_STATUSMSG_DIRLISTSUCCESSFUL, 0);
							SetDirectoryListing(&dir);
							ResetOperation(FZ_REPLY_OK);
							return;
						}
					}
				}
#endif
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
				if((i=retmsg.Find(_T("(")))>=0&&(j=retmsg.Find(_T(")")))>=0)
				{
					i++;
					j--;
				}
				else
				{
					// MP EXT
					if ((i=retmsg.Mid(4).FindOneOf(_T("0123456789")))>=0)
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

#ifdef MPEXT
				temp = retmsg.Mid(i,(j-i)+1);
#endif
				if (GetFamily() == AF_INET)
				{
#ifndef MPEXT
					temp = retmsg.Mid(i,(j-i)+1);
#endif
					i=temp.ReverseFind(_MPT(','));
					pData->port=atol(  T2CA( temp.Right(temp.GetLength()-(i+1)) )  ); //get ls byte of server socket
					temp=temp.Left(i);
					i=temp.ReverseFind(_MPT(','));
					pData->port+=256*atol(  T2CA( temp.Right(temp.GetLength()-(i+1)) )  ); // add ms byte to server socket
					pData->host = temp.Left(i);
					pData->host.Replace(_MPT(','), _MPT('.'));
#ifdef MPEXT
					if (!CheckForcePasvIp(pData->host))
					{
						error = TRUE;
						break;
					}
#endif
				}
				else if (GetFamily() == AF_INET6)
				{
					temp = temp.Mid(3);
					pData->port = atol( T2CA(temp.Left(temp.GetLength() - 1) ) );
					if (pData->port < 0 || pData->port > 65535)
					{
						LogMessage(__FILE__, __LINE__, this, FZ_LOG_WARNING, _T("Port %u not valid"), pData->port);
						error = TRUE;
						break;
					}

					unsigned int tmpPort;
					if (!GetPeerName(pData->host, tmpPort))
					{
						LogMessage(__FILE__, __LINE__, this, FZ_LOG_WARNING, _T("GetPeerName failed"));
						error = TRUE;
						break;
					}
				}
				else
				{
					LogMessage(__FILE__, __LINE__, this, FZ_LOG_WARNING, _T("Protocol %d not supported"), GetFamily());
					error = TRUE;
					break;
				}
			}
			m_Operation.nOpState = LIST_LIST;
			break;
		case LIST_LIST:
			if (IsMisleadingListResponse())
			{
				ShowStatus(IDS_STATUSMSG_DIRLISTSUCCESSFUL, 0);

				t_directory listing;
				listing.server = m_CurrentServer;
				listing.path = m_pOwner->GetCurrentPath();

#ifndef MPEXT_NO_CACHE
				CDirectoryCache cache;
				cache.Lock();
				t_directory dir;
				if (!pData->path.IsEmpty() && pData->subdir != _MPT(""))
				{
					if (cache.Lookup(listing.path, listing.server, dir))
						listing.Merge(dir, pData->ListStartTime);
					cache.Store(listing, pData->path, pData->subdir);
				}
				else
				{
					if (cache.Lookup(listing.path, listing.server, dir))
						listing.Merge(dir, pData->ListStartTime);
					cache.Store(listing);
				}
				cache.Unlock();
#endif
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
		pData->nListMode=nListMode;
		pData->path=path;
		pData->subdir=subdir;
		m_Operation.pData=pData;
		ShowStatus(IDS_STATUSMSG_RETRIEVINGDIRLIST, 0);
		pData->nFinish=-1;
		if (m_pDirectoryListing)
		{
			delete m_pDirectoryListing;
			m_pDirectoryListing=0;
		}

		if (COptions::GetOptionVal(OPTION_PROXYTYPE)!=PROXYTYPE_NOPROXY && !m_CurrentServer.fwbypass)
			pData->bPasv = TRUE;
		else if (m_CurrentServer.nPasv == 1)
			pData->bPasv = TRUE;
		else if (m_CurrentServer.nPasv == 2)
			pData->bPasv = FALSE;
		else
			pData->bPasv = COptions::GetOptionVal(OPTION_PASV);

		CServerPath path = pData->path;
		CServerPath realpath = m_pOwner->GetCurrentPath();
		if (!realpath.IsEmpty())
		{
			if (!pData->path.IsEmpty() && pData->path != realpath)
				m_Operation.nOpState=LIST_CWD;
			else if (!pData->path.IsEmpty() && pData->subdir!=_MPT(""))
				m_Operation.nOpState=LIST_CWD2;
			else
			{
				if (pData->nListMode & FZ_LIST_REALCHANGE)
				{
					if (pData->subdir == _MPT(""))
						m_Operation.nOpState = LIST_CWD;
					else
						m_Operation.nOpState = LIST_CWD2;
				}
				else
				{
#ifndef MPEXT_NO_CACHE
					if (pData->nListMode&FZ_LIST_USECACHE)
					{
						t_directory dir;
						CDirectoryCache cache;
						BOOL res = cache.Lookup(realpath, m_CurrentServer,dir);
						if (res)
						{
							BOOL bExact = TRUE;
							if (pData->nListMode & FZ_LIST_EXACT)
								for (int i = 0; i < dir.num; i++)
									if (dir.direntry[i].bUnsure || (dir.direntry[i].size == -1 && !dir.direntry[i].dir))
									{
										bExact = FALSE;
										break;
									}
							if (bExact)
							{
								ShowStatus(IDS_STATUSMSG_DIRLISTSUCCESSFUL,0);
								SetDirectoryListing(&dir);
								ResetOperation(FZ_REPLY_OK);
								return;
							}
						}
					}
#endif
					m_Operation.nOpState = NeedModeCommand() ? LIST_MODE : (NeedOptsCommand() ? LIST_OPTS : LIST_TYPE);;
				}
			}
		}
		else
			m_Operation.nOpState = LIST_PWD;
	}
	CString cmd;
	if (m_Operation.nOpState == LIST_PWD)
		cmd=_T("PWD");
	else if (m_Operation.nOpState==LIST_CWD)
		cmd=_T("CWD ") + pData->path.GetPath(); //Command to retrieve the current directory
	else if (m_Operation.nOpState==LIST_PWD2)
		cmd=_T("PWD");
	else if (m_Operation.nOpState==LIST_CWD2)
	{
		if (!pData->subdir)
		{
			ResetOperation(FZ_REPLY_ERROR);
			return;
		}
		if (pData->subdir != _T("..") )
		{
			if (m_CurrentServer.nServerType & FZ_SERVERTYPE_SUB_FTP_VMS)
			{
				CServerPath path = m_pOwner->GetCurrentPath();
				path.AddSubdir(pData->subdir);
				cmd = _T("CWD ") + path.GetPath();
			}
			else
				cmd = _T("CWD ") + pData->subdir;
		}
		else
		{
			if (pData->lastCmdSentCDUP)
			{
				pData->lastCmdSentCDUP = false;
				cmd = _T("CWD ..");
			}
			else
			{
				pData->lastCmdSentCDUP = true;
				cmd = _T("CDUP");
			}
		}
	}
	else if (m_Operation.nOpState == LIST_PWD3)
		cmd=_T("PWD");
	else if (m_Operation.nOpState == LIST_MODE)
	{
#ifdef MPEXT_NO_ZLIB
		ASSERT(false);
#else
		if (m_useZlib)
#endif
			cmd = _T("MODE S");
#ifndef MPEXT_NO_ZLIB
		else
			cmd = _T("MODE Z");
#endif
	}
	else if (m_Operation.nOpState == LIST_OPTS)
	{
#ifdef MPEXT_NO_ZLIB
		ASSERT(false);
#else
		pData->newZlibLevel = COptions::GetOptionVal(OPTION_MODEZ_LEVEL);
		cmd.Format(_T("OPTS MODE Z LEVEL %d"), pData->newZlibLevel);
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
				ShowStatus(_T("Failed to initialize zlib"), 1);
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
		if (!m_pTransferSocket->Create(
#ifndef MPEXT_NO_SSL
				m_pSslLayer && m_bProtP
#endif
		) ||
			!m_pTransferSocket->AsyncSelect())
		{
			ShowStatus(_T("Failed to create socket"), 1);
			ResetOperation(FZ_REPLY_ERROR);
			return;
		}
		if (pData->bPasv)
			switch (GetFamily())
			{
			case AF_INET:
				cmd = _T("PASV");
				break;
			case AF_INET6:
				cmd = _T("EPSV");
				break;
			default:
				LogMessage(__FILE__, __LINE__, this, FZ_LOG_WARNING, _T("Protocol %d not supported"), GetFamily());
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
					ShowStatus(_T("Failed to create listen socket"), 1);
					ResetOperation(FZ_REPLY_ERROR);
					return;
				}

				CString host;
				bool bError = false;

				if (GetFamily() == AF_INET)
				{
					host = COptions::GetOption(OPTION_TRANSFERIP);
					if (host != _MPT(""))
					{
						DWORD ip = inet_addr(T2CA(host));
						if (ip != INADDR_NONE)
							host.Format(_T("%d,%d,%d,%d"), ip%256, (ip>>8)%256, (ip>>16)%256, ip>>24);
						else
						{
							hostent *fullname = gethostbyname(T2CA(host));
							if (!fullname)
								host = _MPT("");
							else
							{
								DWORD ip = ((LPIN_ADDR)fullname->h_addr)->s_addr;
								if (ip != INADDR_NONE)
									host.Format(_T("%d,%d,%d,%d"), ip%256, (ip>>8)%256, (ip>>16)%256, ip>>24);
								else
									host = _MPT("");
							}
						}
					}
					if (host == _MPT(""))
					{
						UINT temp;

						if (!GetSockName(host, temp))
						{
							ShowStatus(_T("Failed to get socket address "), 1);
							bError = true;
						}

						host.Replace(_MPT('.'), _MPT(','));
					}

					if (!bError)
					{
						host.Format(host+_MPT(",%d,%d"), nPort/256, nPort%256);
						cmd = _T("PORT ") + host; // send PORT cmd to server
					}
				}
				else if (GetFamily() == AF_INET6)
				{
					host = COptions::GetOption(OPTION_TRANSFERIP6);
					if (host != _MPT(""))
					{
						USES_CONVERSION;
						addrinfo hints, *res;
						memset(&hints, 0, sizeof(addrinfo));
						hints.ai_family = AF_INET6;
						hints.ai_socktype = SOCK_STREAM;
						if (!p_getaddrinfo(T2CA(host), "1024", &hints, &res))
						{
							host = Inet6AddrToString(((SOCKADDR_IN6 *)res->ai_addr)->sin6_addr);
							p_freeaddrinfo(res);
						}
						else
							host = _T("");
					}
					if (host == _MPT(""))
					{
						UINT temp;

						if(!GetSockName(host, temp))
							bError = true;
					}

					if (!bError)
					{
						// assamble EPRT command
						cmd.Format(_T("EPRT |2|") +	host + _MPT("|%d|"), nPort);
					}
				}
				else
				{
					LogMessage(__FILE__, __LINE__, this, FZ_LOG_WARNING, _T("Protocol %d not supported"), GetFamily());
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
		cmd=_T("TYPE A");
	else if (m_Operation.nOpState==LIST_LIST)
	{
		if (!m_pTransferSocket)
		{
			LogMessage(__FILE__, __LINE__, this,FZ_LOG_APIERROR, _T("Error: m_pTransferSocket==NULL") );
			ResetOperation(FZ_REPLY_ERROR);
			return;
		}

		m_pTransferSocket->SetActive();

		cmd = GetListingCmd();
		if (!Send(cmd))
			return;

		pData->ListStartTime=CTime::GetCurrentTime();

		if (pData->bPasv)
		{
			// if PASV create the socket & initiate outbound data channel connection
			if (!m_pTransferSocket->Connect(pData->host,pData->port))
			{
				if (GetLastError()!=WSAEWOULDBLOCK)
				{
					ResetOperation(FZ_REPLY_ERROR);
					return;
				}
			}
		}

		return;
	}
	if (cmd != _T(""))
		Send(cmd);
}

#ifdef MPEXT
void CFtpControlSocket::ListFile(CServerPath path /*=CServerPath()*/, CString fileName /*=""*/)
{
	LogMessage(__FILE__, __LINE__, this,FZ_LOG_DEBUG, _T("ListFile(\"%s\",\"%s\")  OpMode=%d OpState=%d"), path.GetPath(), fileName,
				m_Operation.nOpMode, m_Operation.nOpState);

	USES_CONVERSION;

	#define LIST_LISTFILE	1

	ASSERT(!m_Operation.nOpMode || m_Operation.nOpMode&CSMODE_LISTFILE);

	m_Operation.nOpMode|=CSMODE_LISTFILE;

	if (!m_pOwner->IsConnected())
	{
		ResetOperation(FZ_REPLY_ERROR|FZ_REPLY_NOTCONNECTED);
		return;
	}

	CListData *pData = static_cast<CListData *>(m_Operation.pData);

	BOOL error = FALSE;
	CString cmd;
	CString retmsg;
	int code = -1;
	switch (m_Operation.nOpState)
	{
	case LIST_INIT:
		//Initialize some variables
		pData=new CListData;
		pData->path=path;
		pData->fileName=fileName;
		m_Operation.pData=pData;
		ShowStatus(IDS_STATUSMSG_RETRIEVINGLISTFILE, 0);
		pData->nFinish=-1;
		if (m_pDirectoryListing)
		{
			delete m_pDirectoryListing;
			m_pDirectoryListing=0;
		}
		m_Operation.nOpState = LIST_LISTFILE;
		cmd = _T("MLST ") + path.FormatFilename(fileName);
		if (!Send(cmd))
			return;
		pData->ListStartTime=CTime::GetCurrentTime();
		break;
	case LIST_LISTFILE:
		retmsg = GetReply();
		code = GetReplyCode();
		if (IsMisleadingListResponse())
		{
			ShowStatus(IDS_STATUSMSG_LISTFILESUCCESSFUL, 0);

			t_directory listing;
			listing.server = m_CurrentServer;
			listing.path = m_pOwner->GetCurrentPath();

			SetDirectoryListing(&listing);
			ResetOperation(FZ_REPLY_OK);
			return;
		}
		else if (code != 2)
			error = TRUE;
		else
		{
			USES_CONVERSION;
			int size = m_ListFile.GetLength();
			char *buffer = new char[size + 1];
			memmove(buffer, (LPCSTR)m_ListFile, m_ListFile.GetLength());
			CFtpListResult * pListResult = new CFtpListResult(m_CurrentServer, &m_bUTF8);
			pListResult->InitLog(this);
			pListResult->AddData(buffer, size);
			int num = 0;
			pData->pDirectoryListing = new t_directory;
			if (COptions::GetOptionVal(OPTION_DEBUGSHOWLISTING))
				pListResult->SendToMessageLog(m_pOwner->m_hOwnerWnd, m_pOwner->m_nReplyMessageID);
			pData->pDirectoryListing->direntry = pListResult->getList(num, pData->ListStartTime);
			pData->pDirectoryListing->num = num;
			if (pListResult->m_server.nServerType & FZ_SERVERTYPE_SUB_FTP_VMS && m_CurrentServer.nServerType & FZ_SERVERTYPE_FTP)
				m_CurrentServer.nServerType |= FZ_SERVERTYPE_SUB_FTP_VMS;
			pData->pDirectoryListing->server = m_CurrentServer;
			pData->pDirectoryListing->path.SetServer(pData->pDirectoryListing->server);
			pData->pDirectoryListing->path = m_pOwner->GetCurrentPath();
			delete pListResult;

			ShowStatus(IDS_STATUSMSG_LISTFILESUCCESSFUL,0);
			SetDirectoryListing(pData->pDirectoryListing);
			ResetOperation(FZ_REPLY_OK);
			return;
		}
		break;
	default:
		error = TRUE;
		break;
	}

	if (error)
	{
		ResetOperation(FZ_REPLY_ERROR);
		return;
	}
}
#endif

void CFtpControlSocket::TransferEnd(int nMode)
{
	LogMessage(__FILE__, __LINE__, this,FZ_LOG_DEBUG, _T("TransferEnd(%d/%x)  OpMode=%d OpState=%d"), nMode, nMode, m_Operation.nOpMode, m_Operation.nOpState);
	if (!m_Operation.nOpMode)
	{
		LogMessage(__FILE__, __LINE__, this,FZ_LOG_INFO, _T("Ignoring old TransferEnd message"));
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
	LogMessage(__FILE__, __LINE__, this,FZ_LOG_DEBUG, _T("OnClose(%d)  OpMode=%d OpState=%d"), nErrorCode, m_Operation.nOpMode, m_Operation.nOpState);
	ShowStatus(IDS_STATUSMSG_DISCONNECTED, 1);
	if (m_pTransferSocket)
	{
		m_pTransferSocket->OnClose(0);
		m_pTransferSocket->Close();
		delete m_pTransferSocket;
		m_pTransferSocket=0;
		DoClose();
		ShowStatus(IDS_ERRORMSG_TIMEOUT,1);
		return;
	}
#ifndef MPEXT_NO_SSL
	if (m_bDidRejectCertificate)
		DoClose(FZ_REPLY_CANCEL);
	else
#endif
		DoClose();
}

void CFtpControlSocket::FileTransfer(t_transferfile *transferfile/*=0*/,BOOL bFinish/*=FALSE*/,int nError/*=0*/)
{
	LogMessage(__FILE__, __LINE__, this,FZ_LOG_DEBUG, _T("FileTransfer(%d, %s, %d)  OpMode=%d OpState=%d"), transferfile,bFinish?_T("TRUE"):_T("FALSE"), nError, m_Operation.nOpMode, m_Operation.nOpState);

	USES_CONVERSION;

	#define FILETRANSFER_INIT			-1
	#define FILETRANSFER_PWD			0
	#define FILETRANSFER_CWD			1
	#define FILETRANSFER_MKD			2
	#define FILETRANSFER_CWD2			3
	#define FILETRANSFER_PWD2			4
	#define FILETRANSFER_LIST_MODE		5
	#define FILETRANSFER_LIST_OPTS		6
	#define FILETRANSFER_LIST_PORTPASV	7
	#define FILETRANSFER_LIST_TYPE		8
	#define FILETRANSFER_LIST_LIST		9
	#define FILETRANSFER_LIST_WAITFINISH	10
	#define FILETRANSFER_NOLIST_SIZE	11
	#define FILETRANSFER_NOLIST_MDTM	12
	#define FILETRANSFER_TYPE			13
	#define FILETRANSFER_REST			14
	#define FILETRANSFER_MODE			15
	#define FILETRANSFER_OPTS			16
	#define FILETRANSFER_PORTPASV		17
	#define FILETRANSFER_RETRSTOR		18
	#define FILETRANSFER_WAITFINISH		19

	#define FILETRANSFER_WAIT			20

#ifdef MPEXT
	#define FILETRANSFER_MFMT			21
#endif

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
	//				 |      |
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

	ASSERT(!m_Operation.nOpMode || m_Operation.nOpMode&CSMODE_TRANSFER);
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
		ASSERT(m_Operation.nOpMode&CSMODE_TRANSFER);

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
				// MPEXT: we may get here when connection was closed, when the closure
				// was first detected while reading/writing,
				// when we abort file transfer with regular error,
				// possibly preventing automatic reconnect
				ResetOperation(FZ_REPLY_ERROR);
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
			if (COptions::GetOptionVal(OPTION_DEBUGSHOWLISTING))
				m_pTransferSocket->m_pListResult->SendToMessageLog(m_pOwner->m_hOwnerWnd, m_pOwner->m_nReplyMessageID);
			pData->pDirectoryListing->direntry=m_pTransferSocket->m_pListResult->getList(num, pData->ListStartTime);
			pData->pDirectoryListing->num=num;
			if (m_pTransferSocket->m_pListResult->m_server.nServerType&FZ_SERVERTYPE_SUB_FTP_VMS && m_CurrentServer.nServerType&FZ_SERVERTYPE_FTP)
				m_CurrentServer.nServerType |= FZ_SERVERTYPE_SUB_FTP_VMS;
			pData->pDirectoryListing->server = m_CurrentServer;
			pData->pDirectoryListing->path.SetServer(m_CurrentServer);
			pData->pDirectoryListing->path = pData->transferfile.remotepath;
			if (pData->rawpwd!=_MPT(""))
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
				delete m_pTransferSocket;
				m_pTransferSocket=0;
			}
		}
	}

	//////////////////
	//Initialization//
	//////////////////
	int nReplyError = 0;
	if (m_Operation.nOpState == FILETRANSFER_INIT)
	{
		ASSERT(transferfile);
		ASSERT(!m_Operation.nOpMode);
		ASSERT(!m_Operation.pData);

		CString str;
		str.Format(transferfile->get?IDS_STATUSMSG_DOWNLOADSTART:IDS_STATUSMSG_UPLOADSTART,
					transferfile->get ? transferfile->remotepath.FormatFilename(transferfile->remotefile) : transferfile->localfile);
		ShowStatus(str,0);

		m_Operation.nOpMode=CSMODE_TRANSFER|(transferfile->get?CSMODE_DOWNLOAD:CSMODE_UPLOAD);

		m_Operation.pData=new CFileTransferData;
		pData=static_cast<CFileTransferData *>(m_Operation.pData);

		if (COptions::GetOptionVal(OPTION_PROXYTYPE)!=PROXYTYPE_NOPROXY && !m_CurrentServer.fwbypass)
			pData->bPasv = TRUE;
		else if (m_CurrentServer.nPasv == 1)
			pData->bPasv = TRUE;
		else if (m_CurrentServer.nPasv == 2)
			pData->bPasv = FALSE;
		else
			pData->bPasv = COptions::GetOptionVal(OPTION_PASV);

		//Replace invalid characters in the local filename
		int pos=transferfile->localfile.ReverseFind(_MPT('\\'));
		for (int i=(pos+1);i<transferfile->localfile.GetLength();i++)
			if (transferfile->localfile[i]==_MPT(':'))
				transferfile->localfile.SetAt(i, _MPT('_'));

		pData->transferfile=*transferfile;
		pData->transferdata.transfersize=pData->transferfile.size;
		pData->transferdata.transferleft=pData->transferfile.size;
		pData->transferdata.bResume = FALSE;
		pData->transferdata.bType = (pData->transferfile.nType == 1) ? TRUE : FALSE;

		CServerPath path;
		VERIFY(m_pOwner->GetCurrentPath(path));
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
						m_Operation.nOpState = NeedModeCommand() ? FILETRANSFER_LIST_MODE : (NeedOptsCommand() ? FILETRANSFER_LIST_OPTS : FILETRANSFER_LIST_TYPE);
						break;
					}
				}
				if (m_pDirectoryListing && i==m_pDirectoryListing->num)
				{
					nReplyError = CheckOverwriteFile();
					if (!nReplyError)
					{
						if (pData->transferfile.get)
						{
							CString path=pData->transferfile.localfile;
							if (path.ReverseFind(_MPT('\\'))!=-1)
							{
								path=path.Left(path.ReverseFind(_MPT('\\'))+1);
								CString path2;
								while (path!=_MPT(""))
								{
									path2+=path.Left(path.Find( _T("\\") )+1);
									path=path.Mid(path.Find( _T("\\") )+1);
									CreateDirectory(path2, 0);
								}
							}
						}
					}
				}
			}
			else
			{
#ifndef MPEXT_NO_CACHE
				CDirectoryCache cache;
				t_server server;
				m_pOwner->GetCurrentServer(server);
				t_directory dir;
				BOOL res = cache.Lookup(pData->transferfile.remotepath,server,dir);
				if (res)
				{
					CString remotefile=pData->transferfile.remotefile;
					int i;
					for (i=0; i<dir.num; i++)
					{
						if (dir.direntry[i].name==remotefile &&
							( dir.direntry[i].bUnsure || dir.direntry[i].size==-1 ))
						{
							m_Operation.nOpState = NeedModeCommand() ? FILETRANSFER_LIST_MODE : (NeedOptsCommand() ? FILETRANSFER_LIST_OPTS : FILETRANSFER_LIST_TYPE);
							break;
						}
					}
					if (i == dir.num)
					{
						SetDirectoryListing(&dir);
						m_Operation.nOpState=FILETRANSFER_TYPE;
						nReplyError = CheckOverwriteFile();
						if (!nReplyError)
						{
							if (pData->transferfile.get)
							{
								CString path=pData->transferfile.localfile;
								if (path.ReverseFind(_MPT('\\'))!=-1)
								{
									path=path.Left(path.ReverseFind(_MPT('\\'))+1);
									CString path2;
									while (path!=_MPT(""))
									{
										path2+=path.Left(path.Find( _T("\\") )+1);
										path=path.Mid(path.Find(_T( "\\") )+1);
										CreateDirectory(path2, 0);
									}
								}
							}
						}
					}
				}
				else
#endif
					m_Operation.nOpState = NeedModeCommand() ? FILETRANSFER_LIST_MODE : (NeedOptsCommand() ? FILETRANSFER_LIST_OPTS : FILETRANSFER_LIST_TYPE);
			}
		}
		else
		{
			if (path.IsEmpty())
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
		int code = GetReplyCode();
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
				pData->rawpwd[0] != _MPT('/'))
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
#ifndef MPEXT_NO_CACHE
				CDirectoryCache cache;
				t_server server;
				m_pOwner->GetCurrentServer(server);
				t_directory dir;
				BOOL res = cache.Lookup(pData->transferfile.remotepath,server,dir);
				if (res)
				{
					CString remotefile = pData->transferfile.remotefile;
					int i;
					for (i = 0; i < dir.num; i++)
					{
						if (dir.direntry[i].name == remotefile &&
							(dir.direntry[i].bUnsure || dir.direntry[i].size == -1))
						{
							m_Operation.nOpState = NeedModeCommand() ? FILETRANSFER_LIST_MODE : (NeedOptsCommand() ? FILETRANSFER_LIST_OPTS : FILETRANSFER_LIST_TYPE);
							break;
						}
					}
					if (i == dir.num)
					{
						SetDirectoryListing(&dir);
						m_Operation.nOpState=FILETRANSFER_TYPE;
						nReplyError = CheckOverwriteFile();
						if (!nReplyError)
						{
							if (pData->transferfile.get)
							{
								CString path=pData->transferfile.localfile;
								if (path.ReverseFind(_MPT('\\'))!=-1)
								{
									path=path.Left(path.ReverseFind(_MPT('\\'))+1);
									CString path2;
									while (path!=_MPT(""))
									{
										path2+=path.Left(path.Find( _T("\\") )+1);
										path=path.Mid(path.Find(_T( "\\") )+1);
										int res = CreateDirectory(path2, 0);
									}
								}
							}
						}
					}
				}
				else
#endif
					m_Operation.nOpState = NeedModeCommand() ? FILETRANSFER_LIST_MODE : (NeedOptsCommand() ? FILETRANSFER_LIST_OPTS : FILETRANSFER_LIST_TYPE);
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

#ifndef MPEXT_NO_CACHE
						if (!m_pDirectoryListing)
						{
							CDirectoryCache cache;
							t_directory dir;
							BOOL res = cache.Lookup(pData->MKDCurrent, m_CurrentServer, dir, TRUE);
							if (res)
								SetDirectoryListing(&dir);
						}
#endif

						pData->nMKDOpState=MKD_CHANGETOSUBDIR;
						pData->MKDCurrent.AddSubdir(pData->MKDSegments.front());
						CString Segment=pData->MKDSegments.front();
						pData->MKDSegments.pop_front();
						if (!Send( _T("MKD ") + Segment))
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
							if (!Send(_MPT("CWD ")+pData->MKDCurrent.GetPath()))
								return;
						}
					}
				}
				break;
			case MKD_MAKESUBDIRS:
				{
					if (code == 2 || code == 3)
					{ //Create dir entry in parent dir
						ASSERT(!pData->MKDSegments.empty());
						pData->MKDCurrent.AddSubdir(pData->MKDSegments.front());
						CString Segment=pData->MKDSegments.front();
						pData->MKDSegments.pop_front();
						if (Send( _T("MKD ") + Segment))
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
#ifndef MPEXT_NO_CACHE
							CDirectoryCache cache;
							cache.Lock();
							t_directory dir;
							BOOL bCached=TRUE;
							BOOL res=cache.Lookup(path2,m_CurrentServer,dir);
							if (!res)
								bCached=FALSE;
#else
							t_directory dir;
							BOOL res=FALSE;
#endif
							if (!res && m_pDirectoryListing)
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
									LogMessage(__FILE__, __LINE__, this, FZ_LOG_WARNING, _T("Dir already exists in cache!"));
									break;
								}
							if (i==dir.num)
							{
								t_directory::t_direntry *entries = new t_directory::t_direntry[dir.num+1];
								for (i=0;i<dir.num;i++)
									entries[i]=dir.direntry[i];
								entries[i].name=name;
								entries[i].lName=name;
								entries[i].lName.MakeLower();
								entries[i].dir=TRUE;
								entries[i].date.hasdate=FALSE;
								entries[i].size=-1;
								entries[i].bUnsure=FALSE;
								delete [] dir.direntry;
								dir.direntry=entries;
								dir.num++;
#ifndef MPEXT_NO_CACHE
								cache.Store(dir, bCached);
#endif
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
#ifndef MPEXT_NO_CACHE
							cache.Unlock();
#endif
						}

					}

					//Continue operation even if MKD failed, maybe another thread did create this directory for us
					if (pData->MKDSegments.empty())
						m_Operation.nOpState=FILETRANSFER_CWD2;
					else
					{
						if (Send( _T("CWD ") + pData->MKDCurrent.GetPath()))
							pData->nMKDOpState=MKD_MAKESUBDIRS;
						else
							return;
					}
				}
				break;
			default:
				ASSERT(FALSE);
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

				if (m_pOwner->GetCurrentPath() != pData->transferfile.remotepath)
				{
#ifdef MPEXT
					// More user-friendly message when the actual paths differ
					if (m_pOwner->GetCurrentPath().GetPath() != pData->transferfile.remotepath.GetPath())
					{
						LogMessage(__FILE__, __LINE__, this, FZ_LOG_WARNING, _T("Real path and requested remote path do not match: \"%s\"  \"%s\""), m_pOwner->GetCurrentPath().GetPath(), pData->transferfile.remotepath.GetPath());
					}
					else
#endif
					{
						LogMessage(__FILE__, __LINE__, this, FZ_LOG_WARNING, _T("Real path and requested remote path do not match: \"%s\"  \"%s\""), m_pOwner->GetCurrentPath().GetSafePath(), pData->transferfile.remotepath.GetSafePath());
					}
					nReplyError = FZ_REPLY_CRITICALERROR;
				}
				else
				{
#ifndef MPEXT_NO_CACHE
					CDirectoryCache cache;
					t_server server;
					m_pOwner->GetCurrentServer(server);
					t_directory dir;
					BOOL res = cache.Lookup(pData->transferfile.remotepath, server,dir);
					if (res)
					{
						CString remotefile = pData->transferfile.remotefile;
						int i;
						for (i = 0; i < dir.num; i++)
						{
							if (dir.direntry[i].name == remotefile &&
								(dir.direntry[i].bUnsure || dir.direntry[i].size == -1))
							{
								m_Operation.nOpState = NeedModeCommand() ? FILETRANSFER_LIST_MODE : (NeedOptsCommand() ? FILETRANSFER_LIST_OPTS : FILETRANSFER_LIST_TYPE);
								break;
							}
						}
						if (i == dir.num)
						{
							SetDirectoryListing(&dir);
							m_Operation.nOpState=FILETRANSFER_TYPE;
							nReplyError=CheckOverwriteFile();
							if (!nReplyError)
							{
								if (pData->transferfile.get)
								{
									CString path=pData->transferfile.localfile;
									if (path.ReverseFind(_MPT('\\'))!=-1)
									{
										path=path.Left(path.ReverseFind(_MPT('\\'))+1);
										CString path2;
										while (path!=_MPT(""))
										{
											path2+=path.Left(path.Find( _T("\\") )+1);
											path=path.Mid(path.Find( _T("\\") )+1);
											CreateDirectory(path2, 0);
										}
									}
								}
							}
						}
					}
					else
#endif
						m_Operation.nOpState = NeedModeCommand() ? FILETRANSFER_LIST_MODE : (NeedOptsCommand() ? FILETRANSFER_LIST_OPTS : FILETRANSFER_LIST_TYPE);
				}
			}
			break;
		case FILETRANSFER_LIST_MODE:
#ifdef MPEXT_NO_ZLIB
			ASSERT(false);
			m_Operation.nOpState = FILETRANSFER_LIST_TYPE;
#else
			if (code == 2 || code == 3)
				m_useZlib = !m_useZlib;
			m_Operation.nOpState = NeedOptsCommand() ? FILETRANSFER_LIST_OPTS : FILETRANSFER_LIST_TYPE;
#endif
			break;
		case FILETRANSFER_LIST_OPTS:
#ifdef MPEXT_NO_ZLIB
			ASSERT(false);
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
				if((i=reply.Find(_T("(")))>=0&&(j=reply.Find(_T(")")))>=0)
				{
					i++;
					j--;
				}
				else
				{
					// MP EXT
					if ((i=reply.Mid(4).FindOneOf(_T("0123456789")))>=0)
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

				CString temp;
				// MPEXT
				temp = reply.Mid(i,(j-i)+1);

				if (GetFamily() == AF_INET)
				{
					int count=0;
					int pos=0;
					//Convert commas to dots
					temp.Replace( _T(","), _T(".") );
					while(1)
					{
						pos=temp.Find(_T("."),pos);
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

					i=temp.ReverseFind(_MPT('.'));
					pData->port=atol(  T2CA( temp.Right(temp.GetLength()-(i+1)) )  ); //get ls byte of server socket
					temp=temp.Left(i);
					i=temp.ReverseFind(_MPT('.'));
					pData->port+=256*atol(  T2CA( temp.Right(temp.GetLength()-(i+1)) )  ); // add ms byte to server socket
					pData->host=temp.Left(i);
#ifdef MPEXT
					if (!CheckForcePasvIp(pData->host))
					{
						nReplyError = FZ_REPLY_ERROR;
						break;
					}
#endif
				}
				else if (GetFamily() == AF_INET6)
				{
					temp = temp.Mid(3);
					pData->port = atol( T2CA(temp.Left(temp.GetLength() - 1) ) );
					if (pData->port < 0 || pData->port > 65535)
					{
						LogMessage(__FILE__, __LINE__, this, FZ_LOG_WARNING, _T("Port %u not valid"), pData->port);
						nReplyError = FZ_REPLY_ERROR;
						break;
					}

					unsigned int tmpPort;
					if (!GetPeerName(pData->host, tmpPort))
					{
						LogMessage(__FILE__, __LINE__, this, FZ_LOG_WARNING, _T("GetPeerName failed"));
						nReplyError = FZ_REPLY_ERROR;
						break;
					}
				}

				m_pTransferSocket = new CTransferSocket(this, CSMODE_LIST);
#ifndef MPEXT_NO_ZLIB
				if (m_useZlib)
				{
					if (!m_pTransferSocket->InitZlib(m_zlibLevel))
					{
						ShowStatus(_MPT("Failed to initialize zlib"), 1);
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
				if (!m_pTransferSocket->Create(
#ifndef MPEXT_NO_SSL
						m_pSslLayer && m_bProtP
#endif
					))
				{
					nReplyError = FZ_REPLY_ERROR;
					break;
				}

				VERIFY(m_pTransferSocket->AsyncSelect());
			}
			m_Operation.nOpState=FILETRANSFER_LIST_LIST;
			break;
		case FILETRANSFER_LIST_LIST:
			if (IsMisleadingListResponse())
			{
				ShowStatus(IDS_STATUSMSG_DIRLISTSUCCESSFUL, 0);

				t_directory listing;
				listing.server = m_CurrentServer;
				listing.path.SetServer(m_CurrentServer);
				if (pData->rawpwd != _MPT(""))
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

#ifndef MPEXT_NO_CACHE
				CDirectoryCache cache;
				cache.Lock();
				t_directory dir;
				if (cache.Lookup(listing.path, listing.server, dir))
					listing.Merge(dir, pData->ListStartTime);
				cache.Store(listing);
				cache.Unlock();
#endif
				SetDirectoryListing(&listing);

				m_Operation.nOpState = FILETRANSFER_TYPE;
				delete m_pTransferSocket;
				m_pTransferSocket = 0;

				nReplyError = CheckOverwriteFile();
				if (!nReplyError)
				{
					if (pData->transferfile.get)
					{
						CString path=pData->transferfile.localfile;
						if (path.ReverseFind(_MPT('\\'))!=-1)
						{
							path=path.Left(path.ReverseFind(_MPT('\\'))+1);
							CString path2;
							while (path!=_MPT(""))
							{
								path2+=path.Left(path.Find( _T("\\") )+1);
								path=path.Mid(path.Find( _T("\\") )+1);
								CreateDirectory(path2, 0);
							}
						}
					}
				}
			}
			else if (code==4 || code==5) //LIST failed, try getting file information using SIZE and MDTM
			{
				if (m_pTransferSocket)
					delete m_pTransferSocket;
				m_pTransferSocket=0;
				m_Operation.nOpState = FILETRANSFER_NOLIST_SIZE;
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
					nReplyError=FZ_REPLY_ERROR;
				else
					pData->nGotTransferEndReply = 1;
			}
			if (pData->nGotTransferEndReply && pData->pDirectoryListing)
			{
#ifndef MPEXT_NO_CACHE
				CDirectoryCache cache;
				t_directory dir;
				cache.Lock();
				if (cache.Lookup(pData->pDirectoryListing->path, pData->pDirectoryListing->server, dir, TRUE))
				{
					pData->pDirectoryListing->Merge(dir, pData->ListStartTime);
				}
				cache.Store(*pData->pDirectoryListing);
				cache.Unlock();
#endif
				SetDirectoryListing(pData->pDirectoryListing);
				delete m_pTransferSocket;
				m_pTransferSocket=0;
				m_Operation.nOpState=FILETRANSFER_TYPE;
				nReplyError = CheckOverwriteFile();
				if (!nReplyError)
				{
					if (pData->transferfile.get)
					{
						CString path=pData->transferfile.localfile;
						if (path.ReverseFind(_MPT('\\'))!=-1)
						{
							path=path.Left(path.ReverseFind(_MPT('\\'))+1);
							CString path2;
							while (path!=_MPT(""))
							{
								path2+=path.Left(path.Find( _T("\\") )+1);
								path=path.Mid(path.Find( _T("\\") )+1);
								CreateDirectory(path2, 0);
							}
						}
					}
				}
				pData->nGotTransferEndReply=0;
			}
			break;
		case FILETRANSFER_NOLIST_SIZE:
			if (code==2)
			{
				CString line = GetReply();
				if ( line.GetLength()>4  &&  line.Left(4) == _T("213 ") )
				{
					__int64 size=_ttoi64(line.Mid(4));
					ASSERT(!pData->pFileSize);
					pData->pFileSize=new _int64;
					*pData->pFileSize=size;
				}
			}
			m_Operation.nOpState=FILETRANSFER_NOLIST_MDTM;
			break;
		case FILETRANSFER_NOLIST_MDTM:
			if (code==2)
			{
				CString line = GetReply();
				if ( line.GetLength()>4  &&  line.Left(4) == _T("213 ") )
				{
					int y=0, M=0, d=0, h=0, m=0;
					line=line.Mid(4);
					y=_ttoi(line.Left(4));
					if (y && line.GetLength()>4)
					{
						line=line.Mid(4);
						M=_ttoi(line.Left(2));
						if (M && line.GetLength()>2)
						{
							line=line.Mid(2);
							d=_ttoi(line.Left(2));
							if (d && line.GetLength()>2)
							{
								line=line.Mid(2);
								h=_ttoi(line.Left(2));
								if (h && line.GetLength()>2)
								{
									line=line.Mid(2);
									m=_ttoi(line.Left(2));
									if (m && line.GetLength()>2)
									{
										line=line.Mid(2);
									}
								}
							}
							if (M>0 && M<=12 && d>0 && d<=31 && h>=0 && h<24 && m>=0 && m<60)
							{
							    pData->hasRemoteDate = true;
							    pData->remoteDate.year = y;
							    pData->remoteDate.month = M;
							    pData->remoteDate.day = d;
							    pData->remoteDate.hour = h;
							    pData->remoteDate.minute = m;
							    pData->remoteDate.second = 0;
							    pData->remoteDate.hastime = true;
							    pData->remoteDate.hasseconds = false;
							    pData->remoteDate.hasdate = true;
							    pData->remoteDate.utc = true;
							}
						}
					}
				}
			}
			m_Operation.nOpState=FILETRANSFER_TYPE;
			nReplyError=CheckOverwriteFile();
			break;
		case FILETRANSFER_TYPE:
			if (code!=2 && code!=3)
				nReplyError = FZ_REPLY_ERROR;
			m_Operation.nOpState = NeedModeCommand() ? FILETRANSFER_MODE : (NeedOptsCommand() ? FILETRANSFER_OPTS : FILETRANSFER_PORTPASV);
			break;
		case FILETRANSFER_WAIT:
			if (!pData->nWaitNextOpState)
				nReplyError=FZ_REPLY_ERROR;
			else
				m_Operation.nOpState=pData->nWaitNextOpState;
			break;
		case FILETRANSFER_MODE:
#ifdef MPEXT_NO_ZLIB
			ASSERT(false);
			m_Operation.nOpState = FILETRANSFER_PORTPASV;
#else
			if (code == 2 || code == 3)
				m_useZlib = !m_useZlib;
			m_Operation.nOpState = NeedOptsCommand() ? FILETRANSFER_OPTS : FILETRANSFER_PORTPASV;
#endif
			break;
		case FILETRANSFER_OPTS:
#ifdef MPEXT_NO_ZLIB
			ASSERT(false);
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
					if((i=reply.Find(_T("(")))>=0&&(j=reply.Find(_T(")")))>=0)
					{
						i++;
						j--;
					}
					else
					{
						// MP EXT
						if ((i=reply.Mid(4).FindOneOf(_T("0123456789")))>=0)
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

					CString temp;
					// MPEXT
					temp = reply.Mid(i,(j-i)+1);

					if (GetFamily() == AF_INET)
					{
						int count=0;
						int pos=0;
						//Convert commas to dots
						temp.Replace( _T(","), _T(".") );
						while(1)
						{
							pos=temp.Find( _T("."), pos);
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

						i=temp.ReverseFind(_MPT('.'));
						pData->port=atol(  T2CA( temp.Right(temp.GetLength()-(i+1)) )  ); //get ls byte of server socket
						temp=temp.Left(i);
						i=temp.ReverseFind(_MPT('.'));
						pData->port+=256*atol(  T2CA( temp.Right(temp.GetLength()-(i+1)) )  ); // add ms byte to server socket
						pData->host=temp.Left(i);
#ifdef MPEXT
						if (!CheckForcePasvIp(pData->host))
						{
							nReplyError = FZ_REPLY_ERROR;
							break;
						}
#endif
					}
					else if (GetFamily() == AF_INET6)
					{
						temp = temp.Mid(3);
						pData->port = atol( T2CA(temp.Left(temp.GetLength() - 1) ) );
						if (pData->port < 0 || pData->port > 65535)
						{
							LogMessage(__FILE__, __LINE__, this, FZ_LOG_WARNING, _T("Port %u not valid"), pData->port);
							nReplyError = FZ_REPLY_ERROR;
							break;
						}

						unsigned int tmpPort;
						if (!GetPeerName(pData->host, tmpPort))
						{
							LogMessage(__FILE__, __LINE__, this, FZ_LOG_WARNING, _T("GetPeerName failed"));
							nReplyError = FZ_REPLY_ERROR;
							break;
						}
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
							ShowStatus(_MPT("Failed to initialize zlib"), 1);
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
					if (!m_pTransferSocket->Create(
#ifndef MPEXT_NO_SSL
							m_pSslLayer && m_bProtP
#endif
				 ))
					{
						nReplyError = FZ_REPLY_ERROR;
						break;
					}

					VERIFY(m_pTransferSocket->AsyncSelect());
				}

				if (pData->transferdata.bResume && pData->transferfile.get)
					m_Operation.nOpState = FILETRANSFER_REST;
				else
					m_Operation.nOpState = FILETRANSFER_RETRSTOR;
				BOOL res = FALSE;
				if (!m_pDataFile)
					m_pDataFile = new CFile;
				if (pData->transferfile.get)
				{
					if (pData->transferdata.bResume)
						res = m_pDataFile->Open(pData->transferfile.localfile,CFile::modeCreate|CFile::modeWrite|CFile::modeNoTruncate|CFile::shareDenyWrite);
					else
						res = m_pDataFile->Open(pData->transferfile.localfile,CFile::modeWrite|CFile::modeCreate|CFile::shareDenyWrite);
				}
				else
#ifdef MPEXT
					res = m_pDataFile->Open(pData->transferfile.localfile,CFile::modeRead|CFile::shareDenyNone);
#else
					res = m_pDataFile->Open(pData->transferfile.localfile,CFile::modeRead|CFile::shareDenyWrite);
#endif
				if (!res)
				{
					//Error opening the file
					CString str;
					str.Format(IDS_ERRORMSG_FILEOPENFAILED,pData->transferfile.localfile);
					ShowStatus(str,1);
					nReplyError = FZ_REPLY_ERROR;
					break;
				}

				if (!m_pTransferSocket)
				{
					nReplyError=FZ_REPLY_ERROR;
					break;
				}

				m_pTransferSocket->m_pFile = m_pDataFile;
				if (!pData->transferfile.get)
				{
					pData->transferdata.transfersize=GetLength64(*m_pDataFile);
					pData->transferdata.transferleft=pData->transferdata.transfersize;
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
							ShowStatus(IDS_ERRORMSG_SETFILEPOINTER, 1);
							nReplyError = FZ_REPLY_ERROR;
						}
					}
				}
				else
				{
					pData->transferdata.transfersize=-1;
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
		case FILETRANSFER_REST:
			{ //Resume
				if (code==3 || code==2)
				{
					LONG high = 0;
					pData->transferdata.transferleft = pData->transferdata.transfersize - GetLength64(*m_pDataFile);
					if (SetFilePointer((HANDLE)m_pDataFile->m_hFile, 0, &high, FILE_END)==0xFFFFFFFF && GetLastError()!=NO_ERROR)
					{
						ShowStatus(IDS_ERRORMSG_SETFILEPOINTER, 1);
						nReplyError = FZ_REPLY_ERROR;
					}
					else
						m_Operation.nOpState = FILETRANSFER_RETRSTOR;
				}
				else
				{
					if (code==5 && GetReply()[1]==_MPT('0'))
					{
						if (pData->transferdata.transfersize!=-1 && pData->transferfile.get)
						{
							ASSERT(m_pDataFile);
							if (GetLength64(*m_pDataFile) == pData->transferdata.transfersize)
							{
								ShowStatus(IDS_ERRORMSG_CANTRESUME_FINISH, 0);
								ResetOperation(FZ_REPLY_OK);
								return;
							}
						}

						ShowStatus(IDS_ERRORMSG_CANTRESUME, 1);
						pData->transferdata.transferleft=pData->transferdata.transfersize;
						pData->transferdata.bResume=FALSE;
						m_Operation.nOpState=FILETRANSFER_RETRSTOR;
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
				if (!pData->transferfile.get && pData->transferdata.bResume)
				{
					_int64 nOffset = -1;
					CString reply = GetReply();
					reply.MakeLower();
					int pos = reply.Find(_T("restarting at offset "));
					if (pos != -1)
						pos += _tcslen(_T("restarting at offset "));

					reply = reply.Mid(pos);

					int i;
					for (i=0; i<reply.GetLength(); i++)
					{
						if (reply[i] < _MPT('0') || reply[i] > _MPT('9'))
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
								ShowStatus(IDS_ERRORMSG_SETFILEPOINTER, 1);
								nReplyError = FZ_REPLY_ERROR;
							}
						}
						else
						{
							low=static_cast<LONG>(nOffset&0xFFFFFFFF);
							high=static_cast<LONG>(nOffset>>32);
							if (SetFilePointer((HANDLE)m_pDataFile->m_hFile, low, &high, FILE_BEGIN)==0xFFFFFFFF && GetLastError()!=NO_ERROR)
							{
								ShowStatus(IDS_ERRORMSG_SETFILEPOINTER, 1);
								nReplyError = FZ_REPLY_ERROR;
							}
						}
					}
					if (!nReplyError)
						m_pTransferSocket->SetActive();
				}
				else if (pData->bPasv)
					m_pTransferSocket->SetActive();

			}
			break;
		case FILETRANSFER_WAITFINISH:
			if (!bFinish)
			{
				if (code == 1)
				{
					/* Some non-rfc959 compatible servers send more than one code 1yz reply, especially if using APPE.
					 * Just ignore the additional ones.
					 */
					LogMessage(FZ_LOG_WARNING, _T("Server sent more than one code 1yz reply, ignoring additional reply"));
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
#ifdef MPEXT
		case FILETRANSFER_MFMT:
			//Transfer successful
			ResetOperation(FZ_REPLY_OK);
			break;
#endif
		}
		if (nReplyError)
		{ //Error transferring the file
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
		if (!Send(_MPT("PWD")))
			bError = TRUE;
		break;
	case FILETRANSFER_CWD:
		if (!Send(_MPT("CWD ")+pData->transferfile.remotepath.GetPath()))
			bError=TRUE;
		break;
	case FILETRANSFER_MKD:
		if (pData->nMKDOpState==MKD_INIT)
		{
			if (!pData->transferfile.remotepath.HasParent())
			{
				LogMessage(__FILE__, __LINE__, this,FZ_LOG_WARNING, _T("Can't create root dir"));
				ResetOperation(FZ_REPLY_CRITICALERROR);
				return;
			}
			if (!Send(_MPT("CWD ")+pData->transferfile.remotepath.GetParent().GetPath()))
				bError=TRUE;
			pData->MKDCurrent=pData->transferfile.remotepath.GetParent();
			pData->MKDSegments.push_front(pData->transferfile.remotepath.GetLastSegment());
			pData->nMKDOpState=MKD_FINDPARENT;
		}
		break;
	case FILETRANSFER_CWD2:
		if (!Send(_MPT("CWD ")+pData->transferfile.remotepath.GetPath()))
			bError=TRUE;
		break;
	case FILETRANSFER_PWD2:
		if (!Send(_MPT("PWD")))
			bError=TRUE;
		break;
	case FILETRANSFER_LIST_MODE:
#ifdef MPEXT_NO_ZLIB
		ASSERT(false);
#else
		if (m_useZlib)
		{
			if (!Send(_MPT("MODE S")))
				bError = TRUE;
		}
		else
			if (!Send(_MPT("MODE Z")))
				bError = TRUE;
#endif
		break;
	case FILETRANSFER_LIST_OPTS:
#ifdef MPEXT_NO_ZLIB
		ASSERT(false);
#else
		{
			pData->newZlibLevel = COptions::GetOptionVal(OPTION_MODEZ_LEVEL);
			CString str;
			str.Format(_T("OPTS MODE Z LEVEL %d"), pData->newZlibLevel);
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
			if (!Send((GetFamily() == AF_INET) ? _MPT("PASV") : _MPT("EPSV")))
				bError=TRUE;
		}
		else
		{
			if (m_pTransferSocket)
			{
				LogMessage(__FILE__, __LINE__, this, FZ_LOG_WARNING, _T("m_pTransferSocket != 0"));
				delete m_pTransferSocket;
			}
			m_pTransferSocket = new CTransferSocket(this, CSMODE_LIST);
#ifndef MPEXT_NO_ZLIB
			if (m_useZlib)
			{
				if (!m_pTransferSocket->InitZlib(m_zlibLevel))
				{
					ShowStatus(_MPT("Failed to initialize zlib"), 1);
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
			if(!m_pTransferSocket->Create(
#ifndef MPEXT_NO_SSL
					m_pSslLayer && m_bProtP
#endif
				) || !m_pTransferSocket->AsyncSelect())
				bError=TRUE;
			else if (m_pProxyLayer)
			{
				SOCKADDR_IN addr;
				int len=sizeof(addr);
				if (!m_pProxyLayer->GetPeerName((SOCKADDR *)&addr,&len))
				{
					ShowStatus(IDS_ERRORMSG_CANTGETLIST,1);
					bError=TRUE;
				}
				else if (!m_pTransferSocket->Listen(addr.sin_addr.S_un.S_addr))
				{
					ShowStatus(IDS_ERRORMSG_CANTGETLIST,1);
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
					host = COptions::GetOption(OPTION_TRANSFERIP);
					if (host != _MPT(""))
					{
						DWORD ip = inet_addr(T2CA(host));
						if (ip != INADDR_NONE)
							host.Format(_T("%d,%d,%d,%d"), ip%256, (ip>>8)%256, (ip>>16)%256, ip>>24);
						else
						{
							hostent *fullname = gethostbyname(T2CA(host));
							if (!fullname)
								host = _MPT("");
							else
							{
								DWORD ip = ((LPIN_ADDR)fullname->h_addr)->s_addr;
								if (ip != INADDR_NONE)
									host.Format(_T("%d,%d,%d,%d"), ip%256, (ip>>8)%256, (ip>>16)%256, ip>>24);
								else
									host = _MPT("");
							}
						}
					}
					if (host == _MPT(""))
					{
						UINT temp;

						if (!GetSockName(host, temp))
						{
							bError = true;
							break;
						}

						host.Replace(_MPT('.'), _MPT(','));
					}

					if (!bError)
					{
						host.Format(host+_MPT(",%d,%d"), nPort/256, nPort%256);
						if (!Send(_T("PORT ") + host)) // send PORT cmd to server
							bError = TRUE;
					}
				}
				else if (GetFamily() == AF_INET6)
				{
					host = COptions::GetOption(OPTION_TRANSFERIP6);
					if (host != _MPT(""))
					{
						USES_CONVERSION;
						addrinfo hints, *res;
						memset(&hints, 0, sizeof(addrinfo));
						hints.ai_family = AF_INET6;
						hints.ai_socktype = SOCK_STREAM;
						if (!p_getaddrinfo(T2CA(host), "1024", &hints, &res))
						{
							host = Inet6AddrToString(((SOCKADDR_IN6 *)res->ai_addr)->sin6_addr);
							p_freeaddrinfo(res);
						}
						else
							host = _T("");
					}
					if (host == _MPT(""))
					{
						UINT temp;

						if(!GetSockName(host, temp))
							bError = true;
					}

					if (!bError)
					{
						// assamble EPRT command
						CString cmd;
						cmd.Format(_T("EPRT |2|") +	host + _MPT("|%d|"), nPort);
						if (!Send(cmd))
							bError = TRUE;
					}
				}
				else
				{
					LogMessage(__FILE__, __LINE__, this, FZ_LOG_WARNING, _T("Protocol %d not supported"), GetFamily());
					bError = true;
				}
			}
		}
		break;
	case FILETRANSFER_LIST_TYPE:
		if (!Send(_MPT("TYPE A")))
			bError=TRUE;
		break;
	case FILETRANSFER_LIST_LIST:
		{
			if (!m_pTransferSocket)
			{
				LogMessage(__FILE__, __LINE__, this,FZ_LOG_APIERROR, _T("Error: m_pTransferSocket==NULL") );
				ResetOperation(FZ_REPLY_ERROR);
				return;
			}

			m_pTransferSocket->SetActive();
			pData->ListStartTime=CTime::GetCurrentTime();
			CString cmd = GetListingCmd();
			if(!Send(cmd))
				bError=TRUE;
			else if(pData->bPasv)
			{
				// if PASV create the socket & initiate outbound data channel connection
				if (!m_pTransferSocket->Connect(pData->host,pData->port))
				{
					if (GetLastError()!=WSAEWOULDBLOCK)
					{
						bError=TRUE;
						ShowStatus(IDS_ERRORMSG_CANTGETLIST,1);
					}
				}
			}
		}
		break;
	case FILETRANSFER_NOLIST_SIZE:
		{
			CString command = _T("SIZE ");
			command += pData->transferfile.remotepath.FormatFilename(pData->transferfile.remotefile, !pData->bUseAbsolutePaths);

			if (!Send(command))
				bError=TRUE;
		}
		break;
	case FILETRANSFER_NOLIST_MDTM:
		{
			CString command = _T("MDTM ");
			command += pData->transferfile.remotepath.FormatFilename(pData->transferfile.remotefile, !pData->bUseAbsolutePaths);

			if (!Send(command))
				bError=TRUE;
		}
		break;
	case FILETRANSFER_TYPE:
		if (pData->transferfile.nType==1)
		{
			if (!Send(_MPT("TYPE A")))
				bError=TRUE;
		}
		else
			if (!Send(_MPT("TYPE I")))
				bError=TRUE;
		break;
	case FILETRANSFER_MODE:
#ifdef MPEXT_NO_ZLIB
		ASSERT(false);
#else
		if (m_useZlib)
		{
			if (!Send(_MPT("MODE S")))
				bError = TRUE;
		}
		else
			if (!Send(_MPT("MODE Z")))
				bError = TRUE;
#endif
		break;
	case FILETRANSFER_OPTS:
#ifdef MPEXT_NO_ZLIB
		ASSERT(false);
#else
		{
			pData->newZlibLevel = COptions::GetOptionVal(OPTION_MODEZ_LEVEL);
			CString str;
			str.Format(_T("OPTS MODE Z LEVEL %d"), pData->newZlibLevel);
			if (!Send(str))
				bError = TRUE;
		}
#endif
		break;
	case FILETRANSFER_PORTPASV:
		if (pData->bPasv)
		{
			if (!Send((GetFamily() == AF_INET) ? _MPT("PASV") : _MPT("EPSV")))
				bError=TRUE;
		}
		else
		{
			if (m_pTransferSocket)
			{
				LogMessage(__FILE__, __LINE__, this, FZ_LOG_WARNING, _T("m_pTransferSocket != 0"));
				delete m_pTransferSocket;
			}
			m_pTransferSocket=new CTransferSocket(this, m_Operation.nOpMode);
#ifndef MPEXT_NO_ZLIB
			if (m_useZlib)
			{
				if (!m_pTransferSocket->InitZlib(m_zlibLevel))
				{
					ShowStatus(_MPT("Failed to initialize zlib"), 1);
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
			if(!m_pTransferSocket->Create(
#ifndef MPEXT_NO_SSL
					m_pSslLayer && m_bProtP
#endif
				) || !m_pTransferSocket->AsyncSelect())
				bError = TRUE;
			else if (m_pProxyLayer)
			{
				SOCKADDR_IN addr;
				int len=sizeof(addr);
				if (!m_pProxyLayer->GetPeerName((SOCKADDR *)&addr,&len))
				{
					ShowStatus(IDS_ERRORMSG_CANTGETLIST,1);
					bError=TRUE;
				}
				else if (!m_pTransferSocket->Listen(addr.sin_addr.S_un.S_addr))
				{
					ShowStatus(IDS_ERRORMSG_CANTGETLIST,1);
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
					host = COptions::GetOption(OPTION_TRANSFERIP);
					if (host != _MPT(""))
					{
						DWORD ip = inet_addr(T2CA(host));
						if (ip != INADDR_NONE)
							host.Format(_T("%d,%d,%d,%d"), ip%256, (ip>>8)%256, (ip>>16)%256, ip>>24);
						else
						{
							hostent *fullname = gethostbyname(T2CA(host));
							if (!fullname)
								host = _MPT("");
							else
							{
								DWORD ip = ((LPIN_ADDR)fullname->h_addr)->s_addr;
								if (ip != INADDR_NONE)
									host.Format(_T("%d,%d,%d,%d"), ip%256, (ip>>8)%256, (ip>>16)%256, ip>>24);
								else
									host = _MPT("");
							}
						}
					}
					if (host == _MPT(""))
					{
						UINT temp;

						if (!GetSockName(host, temp))
							bError = true;

						host.Replace(_MPT('.'), _MPT(','));
					}

					if (!bError)
					{
						host.Format(host+_MPT(",%d,%d"), nPort/256, nPort%256);
						if (!Send(_T("PORT ") + host)) // send PORT cmd to server
							bError = TRUE;
					}
				}
				else if (GetFamily() == AF_INET6)
				{
					host = COptions::GetOption(OPTION_TRANSFERIP6);
					if (host != _MPT(""))
					{
						USES_CONVERSION;
						addrinfo hints, *res;
						memset(&hints, 0, sizeof(addrinfo));
						hints.ai_family = AF_INET6;
						hints.ai_socktype = SOCK_STREAM;
						if (!p_getaddrinfo(T2CA(host), "1024", &hints, &res))
						{
							host = Inet6AddrToString(((SOCKADDR_IN6 *)res->ai_addr)->sin6_addr);
							p_freeaddrinfo(res);
						}
						else
							host = _T("");
					}
					if (host == _MPT(""))
					{
						UINT temp;

						if(!GetSockName(host, temp))
							bError = true;
					}

					if (!bError)
					{
						// assamble EPRT command
						CString cmd;
						cmd.Format(_T("EPRT |2|") +	host + _MPT("|%d|"), nPort);
						if (!Send(cmd))
							bError = TRUE;
					}
				}
				else
				{
					LogMessage(__FILE__, __LINE__, this, FZ_LOG_WARNING, _T("Protocol %d not supported"), GetFamily());
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
	case FILETRANSFER_REST:
		ASSERT(m_pDataFile);
		{
			CString command;
			command.Format(_T("REST %I64d"), GetLength64(*m_pDataFile));
			if (!Send(command))
				bError=TRUE;
		}
		break;
	case FILETRANSFER_RETRSTOR:
		pData->transferdata.nTransferStart=pData->transferdata.transfersize-pData->transferdata.transferleft;
		// send RETR/STOR command to server
		if (!m_pTransferSocket)
		{
			LogMessage(__FILE__, __LINE__, this,FZ_LOG_APIERROR, _T("Error: m_pTransferSocket==NULL") );
			ResetOperation(FZ_REPLY_ERROR);
			return;
		}
		m_pTransferSocket->m_transferdata=pData->transferdata;
		if ((pData->transferfile.get || !pData->transferdata.bResume) && !pData->bPasv)
			m_pTransferSocket->SetActive();
		CString filename;

		filename = pData->transferfile.remotepath.FormatFilename(pData->transferfile.remotefile, !pData->bUseAbsolutePaths);
		if(!Send((pData->transferfile.get?_MPT("RETR "):(pData->transferdata.bResume)?_MPT("APPE "):_MPT("STOR "))+ filename))
			bError = TRUE;
		else
		{
			if (pData->bPasv)
			{// if PASV create the socket & initiate outbound data channel connection
				if (!m_pTransferSocket->Connect(pData->host,pData->port))
				{
					if (GetLastError()!=WSAEWOULDBLOCK)
						bError=TRUE;
				}
			}
		}
		break;
	}
	if (bError)
	{ //Error transferring the file
		ResetOperation(FZ_REPLY_ERROR);
		return;
	}
}

void CFtpControlSocket::TransferFinished(bool preserveFileTimeForUploads)
{
	CFileTransferData *pData=static_cast<CFileTransferData *>(m_Operation.pData);

	if (COptions::GetOptionVal(OPTION_PRESERVEDOWNLOADFILETIME) && m_pDataFile &&
				pData->transferfile.get)
	{
		#ifdef MPEXT
		m_pTools->PreserveDownloadFileTime(
			(HANDLE)m_pDataFile->m_hFile, reinterpret_cast<void *>(pData->transferfile.nUserData));
		#else
		if (pData->pFileTime)
		{
			SYSTEMTIME stime;
			FILETIME ftime;
			if (pData->pFileTime->GetAsSystemTime(stime))
				if (SystemTimeToFileTime(&stime, &ftime))
					SetFileTime((HANDLE)m_pDataFile->m_hFile, &ftime, &ftime, &ftime);
		}
		#endif
	}
#ifdef MPEXT
	if (!pData->transferfile.get &&
			COptions::GetOptionVal(OPTION_MPEXT_PRESERVEUPLOADFILETIME) && preserveFileTimeForUploads &&
			(m_serverCapabilities.GetCapability(mfmt_command) == yes))
	{
		CString filename =
			pData->transferfile.remotepath.FormatFilename(pData->transferfile.remotefile, !pData->bUseAbsolutePaths);
		struct tm tm;
		if (m_pTools->GetFileModificationTimeInUtc((LPCTSTR)pData->transferfile.localfile, tm))
		{
			CString timestr;
			timestr.Format(L"%02d%02d%02d%02d%02d%02d",
				1900 + tm.tm_year, 1 + tm.tm_mon, tm.tm_mday, tm.tm_hour, tm.tm_min, tm.tm_sec);
			if (Send( _T("MFMT ") + timestr + _T(" ") + filename))
			{
				m_Operation.nOpState = FILETRANSFER_MFMT;
				return;
			}
		}
	}
#endif
	//Transfer successful
	ResetOperation(FZ_REPLY_OK);
}

void CFtpControlSocket::Cancel(BOOL bQuit/*=FALSE*/)
{
	LogMessage(__FILE__, __LINE__, this,FZ_LOG_DEBUG, _T("Cancel(%s)  OpMode=%d OpState=%d"), bQuit?_T("TRUE"):_T("FALSE"), m_Operation.nOpMode, m_Operation.nOpState);

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
		ShowStatus(IDS_ERRORMSG_INTERRUPTED, 1);

	if (m_awaitsReply)
		m_skipReply = true;
}

void CFtpControlSocket::TransfersocketListenFinished(unsigned int ip, unsigned short port)
{
	if (m_Operation.nOpMode&CSMODE_TRANSFER || m_Operation.nOpMode&CSMODE_LIST)
	{
		CString host;
		host.Format(_T("%d,%d,%d,%d,%d,%d"),ip%256,(ip>>8)%256,(ip>>16)%256,(ip>>24)%256,port%256,port>>8);
		Send(_MPT("PORT ")+host);
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
	if (!COptions::GetOptionVal(OPTION_LIMITPORTRANGE))
		return CAsyncSocketEx::Create(0, SOCK_STREAM, FD_READ | FD_WRITE | FD_OOB | FD_ACCEPT |	FD_CONNECT | FD_CLOSE, 0, COptions::GetOptionVal(OPTION_ENABLE_IPV6) ? AF_UNSPEC : AF_INET);
	else
	{
		int min=COptions::GetOptionVal(OPTION_PORTRANGELOW);
		int max=COptions::GetOptionVal(OPTION_PORTRANGEHIGH);
		if (min>=max)
		{
			ShowStatus(IDS_ERRORMSG_CANTCREATEDUETOPORTRANGE,1);
			return FALSE;
		}
		int startport = static_cast<int>(min+((double)rand()*(max-min))/(RAND_MAX+1));
		int port = startport;

		while (!CAsyncSocketEx::Create(port, SOCK_STREAM, FD_READ | FD_WRITE | FD_OOB | FD_ACCEPT |	FD_CONNECT | FD_CLOSE, 0, COptions::GetOptionVal(OPTION_ENABLE_IPV6) ? AF_UNSPEC : AF_INET))
		{
			port++;
			if (port>max)
				port=min;
			if (port==startport)
			{
				ShowStatus(IDS_ERRORMSG_CANTCREATEDUETOPORTRANGE,1);
				return FALSE;
			}

			if (!InitConnect())
				return FALSE; 
		}
	}
	return TRUE;
}


void CFtpControlSocket::ResetOperation(int nSuccessful /*=FALSE*/)
{
	LogMessage(__FILE__, __LINE__, this,FZ_LOG_DEBUG, _T("ResetOperation(%d)  OpMode=%d OpState=%d"), nSuccessful, m_Operation.nOpMode, m_Operation.nOpState);

	if (nSuccessful & FZ_REPLY_CRITICALERROR)
		nSuccessful |= FZ_REPLY_ERROR;

	if (m_pTransferSocket)
		delete m_pTransferSocket;
	m_pTransferSocket=0;

#ifndef MPEXT_NO_IDENT
	//There may be an active ident socket, close it
	if (m_pIdentControl)
	{
		delete m_pIdentControl;
		m_pIdentControl=0;
	}
#endif

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
			ShowStatus(IDS_ERRORMSG_CANTCONNECT, 1);
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
#ifndef MPEXT_NO_CACHE
			CDirectoryCache cache;
			cache.Lock();
			t_directory dir;
			BOOL bCached=TRUE;
			BOOL res=cache.Lookup(path, m_CurrentServer, dir);
			if (!res)
				bCached=FALSE;
#else
			t_directory dir;
			BOOL res=FALSE;
#endif
			if (!res && m_pDirectoryListing)
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
					entries[i].lName=filename;
					entries[i].lName.MakeLower();
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
#ifndef MPEXT_NO_CACHE
				cache.Store(dir, bCached);
#endif
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
#ifndef MPEXT_NO_CACHE
			cache.Unlock();
#endif
		}

		if (m_Operation.pData && nSuccessful&FZ_REPLY_ERROR)
		{
			if (m_Operation.nOpMode&CSMODE_TRANSFER)
				if (nSuccessful&FZ_REPLY_ABORTED)
					//Transfer aborted by user
					ShowStatus((m_Operation.nOpMode&CSMODE_DOWNLOAD)?IDS_ERRORMSG_DOWNLOADABORTED:IDS_ERRORMSG_UPLOADABORTED,1);
				else
					ShowStatus(((CFileTransferData*)m_Operation.pData)->transferfile.get?IDS_ERRORMSG_DOWNLOADFAILED:IDS_ERRORMSG_UPLOADFAILED,1);
			else if (m_Operation.nOpMode&CSMODE_LIST)
				ShowStatus(IDS_ERRORMSG_CANTGETLIST,1);
			else if (m_Operation.nOpMode&CSMODE_LISTFILE)
				ShowStatus(IDS_ERRORMSG_CANTGETLISTFILE,1);
		}
		else if (m_Operation.pData && m_Operation.nOpMode&CSMODE_TRANSFER && nSuccessful==FZ_REPLY_OK)
			ShowStatus(((CFileTransferData*)m_Operation.pData)->transferfile.get?IDS_STATUSMSG_DOWNLOADSUCCESSFUL:IDS_STATUSMSG_UPLOADSUCCESSFUL,0);
	}
	else
	{
#ifdef MPEXT
		// When control socket is waiting for reply
		// to keepalive (!IsReady), while new command comes,
		// its execution is postponed
		// (CMainThread::m_pPostKeepAliveCommand),
		// but the main thread
		// is set to busy state already (CMainThread::Command).
		// if connection is closed before without receiving reply,
		// main thread would stay busy forever.
		m_pOwner->SetBusy(FALSE);
#endif
		//No operation in progress
		nSuccessful&=FZ_REPLY_DISCONNECTED|FZ_REPLY_CANCEL;
		if (!nSuccessful)
			ASSERT(FALSE);
	}

	if (nSuccessful&FZ_REPLY_DISCONNECTED)
		m_pOwner->SetWorkingDir(0); //Disconnected, reset working dir

	if (m_Operation.nOpMode)
		PostMessage(m_pOwner->m_hOwnerWnd, m_pOwner->m_nReplyMessageID, FZ_MSG_MAKEMSG(FZ_MSG_REPLY, m_pOwner->m_LastCommand.id), nSuccessful);
	else
		PostMessage(m_pOwner->m_hOwnerWnd, m_pOwner->m_nReplyMessageID, FZ_MSG_MAKEMSG(FZ_MSG_REPLY, 0), nSuccessful);

	m_Operation.nOpMode=0;
	m_Operation.nOpState=-1;

	if (m_Operation.pData)
		delete m_Operation.pData;
	m_Operation.pData=0;
}

void CFtpControlSocket::Delete(CString filename, const CServerPath &path)
{
	LogMessage(__FILE__, __LINE__, this,FZ_LOG_DEBUG, _T("Delete(\"%s\", \"%s\")  OpMode=%d OpState=%d"), filename, path.GetPath(), m_Operation.nOpMode, m_Operation.nOpState);

	class CDeleteData : public CFtpControlSocket::t_operation::COpData
	{
public:
		CDeleteData() {}
		virtual ~CDeleteData() {}
		CString m_FileName;
		CServerPath path;
	};
	if (filename!=_MPT(""))
	{
		ASSERT(!path.IsEmpty());
		ASSERT(m_Operation.nOpMode==CSMODE_NONE);
		ASSERT(m_Operation.nOpState==-1);
		ASSERT(!m_Operation.pData);
		m_Operation.nOpMode=CSMODE_DELETE;
		if (!Send(_MPT("DELE ") + path.FormatFilename(filename)))
			return;
		CDeleteData *data=new CDeleteData;
		data->m_FileName=filename;
		data->path=path;
		m_Operation.pData=data;
	}
	else
	{
		ASSERT(path.IsEmpty());
		ASSERT(m_Operation.nOpMode==CSMODE_DELETE);
		ASSERT(m_Operation.nOpState==-1);
		ASSERT(m_Operation.pData);
		int res=GetReplyCode();
		if (res==2 || res==3)
		{ //Remove file from cached dirs
			CDeleteData *pData=(CDeleteData *)m_Operation.pData;
#ifndef MPEXT_NO_CACHE
			CDirectoryCache cache;
			cache.Lock();
			BOOL bCached=TRUE;
			t_directory dir;
			BOOL res=cache.Lookup(pData->path,m_CurrentServer,dir);
			if (!res)
				bCached=FALSE;
#else
			t_directory dir;
			BOOL res=FALSE;
#endif
			if (!res && m_pDirectoryListing)
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
						ASSERT(!dir.direntry[i].dir || dir.direntry[i].bLink);
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
#ifndef MPEXT_NO_CACHE
					cache.Store(dir, bCached);
#endif
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
#ifndef MPEXT_NO_CACHE
			cache.Unlock();
#endif
		}
		ResetOperation(FZ_REPLY_OK);
	}
}

void CFtpControlSocket::RemoveDir(CString dirname, const CServerPath &path)
{
	LogMessage(__FILE__, __LINE__, this,FZ_LOG_DEBUG, _T("RemoveDir(\"%s\", \"%s\")  OpMode=%d OpState=%d"), dirname, path.GetPath(), m_Operation.nOpMode, m_Operation.nOpState);

	class CRemoveDirData : public CFtpControlSocket::t_operation::COpData
	{
public:
		CRemoveDirData() {}
		virtual ~CRemoveDirData() {}
		CString m_DirName;
		CServerPath path;
	};
	if (dirname != _MPT(""))
	{
		ASSERT(!path.IsEmpty());
		ASSERT(m_Operation.nOpMode == CSMODE_NONE);
		ASSERT(m_Operation.nOpState == -1);
		ASSERT(!m_Operation.pData);
		m_Operation.nOpMode = CSMODE_RMDIR;
		CServerPath newPath = path;
		if (!newPath.AddSubdir(dirname))
		{
			ShowStatus(_T("Unable to concatenate path"), 1);
			return;
		}
		if (!Send(_MPT("RMD ")+ newPath.GetPath()))
			return;
		CRemoveDirData *data = new CRemoveDirData;
		data->m_DirName = dirname;
		data->path = path;
		m_Operation.pData = data;
	}
	else
	{
		ASSERT(path.IsEmpty());
		ASSERT(m_Operation.nOpMode == CSMODE_RMDIR);
		ASSERT(m_Operation.nOpState == -1);
		ASSERT(m_Operation.pData);
		int res = GetReplyCode();
		if (res == 2 || res == 3)
		{ //Remove dir from cached dirs
			CRemoveDirData *pData=  (CRemoveDirData *)m_Operation.pData;
#ifndef MPEXT_NO_CACHE
			CDirectoryCache cache;
			cache.Lock();
			BOOL bCached = TRUE;
			t_directory dir;
			BOOL res = cache.Lookup(pData->path, m_CurrentServer, dir);
			if (!res)
				bCached = FALSE;
#else
			t_directory dir;
			BOOL res = FALSE;
#endif
			if (!res && m_pDirectoryListing)
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
						ASSERT(dir.direntry[i].dir);
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
#ifndef MPEXT_NO_CACHE
					cache.Store(dir, bCached);
#endif
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
#ifndef MPEXT_NO_CACHE
			if (cache.Lookup(pData->path, pData->m_DirName, m_CurrentServer, dir))
				cache.Purge(dir.path, dir.server);
			cache.Unlock();
#endif
			ResetOperation(FZ_REPLY_OK);
		}
		else
			ResetOperation(FZ_REPLY_ERROR);
	}
}

int CFtpControlSocket::CheckOverwriteFile()
{
	if (!m_Operation.pData)
		return FZ_REPLY_ERROR;

	CFileTransferData *pData = reinterpret_cast<CFileTransferData *>(m_Operation.pData);

	int nReplyError = 0;
	CFileStatus64 status;
	BOOL res = GetStatus64(pData->transferfile.localfile, status);
	if (!res)
		if (!pData->transferfile.get)
			nReplyError = FZ_REPLY_CRITICALERROR; //File has to exist when uploading
		else
			m_Operation.nOpState = FILETRANSFER_TYPE;
	else
	{
		if (status.m_attribute & 0x10)
			nReplyError = FZ_REPLY_CRITICALERROR; //Can't transfer to/from dirs
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
				CString str =_T("Exception creating CTime object: ");
				if (e->GetErrorMessage(buffer, 1024, NULL))
					str += buffer;
				else
					str += _T("Unknown exception");
				LogMessageRaw(__FILE__, __LINE__, this, FZ_LOG_WARNING, str);
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
					int pos = pData->transferfile.localfile.ReverseFind(_MPT('\\'));
					ASSERT(pos!=-1);
					pOverwriteData->FileName1 = pData->transferfile.localfile.Mid(pos+1);
					pOverwriteData->FileName2 = pData->transferfile.remotefile;
					pOverwriteData->path1 = pData->transferfile.localfile.Left(pos+1);
					pOverwriteData->path2 = pData->transferfile.remotepath.GetPath();
					pOverwriteData->size1 = localsize;
					pOverwriteData->size2 = remotesize;
				}
				else
				{
					int pos = pData->transferfile.localfile.ReverseFind(_MPT('\\'));
					ASSERT(pos!=-1);
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
				if (!PostMessage(m_pOwner->m_hOwnerWnd, m_pOwner->m_nReplyMessageID, FZ_MSG_MAKEMSG(FZ_MSG_ASYNCREQUEST, FZ_ASYNCREQUEST_OVERWRITE), (LPARAM)pOverwriteData))
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
	case FILEEXISTS_OVERWRITEIFNEWER:
		// MPEXT
		ASSERT(FALSE);
		nReplyError = FZ_REPLY_OK;
		break;
	case FILEEXISTS_RENAME:
		if (pTransferData->transferfile.get)
		{
			CFileStatus64 status;
			if (GetStatus64(pData->FileName1, status))
			{
				ShowStatus(IDS_ERRORMSG_NAMEINUSE, 1);
				nReplyError=  FZ_REPLY_CRITICALERROR;
			}
			else
			{
				pTransferData->transferfile.localfile = pData->path1+pData->FileName1;
				//Replace invalid characters in the local filename
				int pos = pTransferData->transferfile.localfile.ReverseFind(_MPT('\\'));
				for (int i = (pos+1); i < pTransferData->transferfile.localfile.GetLength(); i++)
					if (pTransferData->transferfile.localfile[i] == _MPT(':'))
						pTransferData->transferfile.localfile.SetAt(i, _MPT('_'));

				pTransferData->nWaitNextOpState=  FILETRANSFER_TYPE;
			}
		}
		else
		{
			ASSERT(m_pDirectoryListing);
			int i;
			for (i = 0; i < m_pDirectoryListing->num; i++)
			{
				if (m_pDirectoryListing->direntry[i].name == pData->FileName1)
				{
					ShowStatus(IDS_ERRORMSG_NAMEINUSE, 1);
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
	case FILEEXISTS_RESUME_ASKONFAIL:
		pTransferData->askOnResumeFail = true;
	case FILEEXISTS_RESUME:
		if (pData->size1 >= 0)
		{
			pTransferData->transferdata.bResume = TRUE;
		}
		pTransferData->nWaitNextOpState = FILETRANSFER_TYPE;
		break;
	#ifdef MPEXT
	case FILEEXISTS_COMPLETE:
		// Simulating transfer finish
		m_Operation.nOpState=FILETRANSFER_WAITFINISH;
		TransferFinished(true);
		return; // Avoid call to FileTransfer below
	}
	#endif
	if (nReplyError == FZ_REPLY_OK)
		ResetOperation(FZ_REPLY_OK);
	else if (nReplyError)
		ResetOperation(FZ_REPLY_ERROR | nReplyError); //Error transferring the file
	else
		FileTransfer();
}

void CFtpControlSocket::SendKeepAliveCommand()
{
	m_bKeepAliveActive=TRUE;
	//Choose a random command from the list
	TCHAR commands[4][7]={_MPT("PWD"),_MPT("REST 0"),_MPT("TYPE A"),_MPT("TYPE I")};
	int choice=(rand()*4)/(RAND_MAX+1);
	Send(commands[choice]);
}

void CFtpControlSocket::MakeDir(const CServerPath &path)
{
	//Directory creation works like this:
	//Find existing parent and create subdirs one by one
	LogMessage(__FILE__, __LINE__, this, FZ_LOG_DEBUG, _T("MakeDir(\"%s\")  OpMode=%d OpState=%d"), path.GetPath(), m_Operation.nOpMode, m_Operation.nOpState);

	if (m_Operation.nOpState == MKD_INIT)
	{
		ASSERT(!path.IsEmpty());
		ASSERT(m_Operation.nOpMode==CSMODE_NONE);
		ASSERT(!m_Operation.pData);
		m_Operation.nOpMode = CSMODE_MKDIR;
		if (!Send(_MPT("CWD ")+path.GetParent().GetPath()))
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
		ASSERT(m_Operation.nOpMode==CSMODE_MKDIR);
		ASSERT(path.IsEmpty());
		ASSERT(m_Operation.pData);
		CMakeDirData *pData=(CMakeDirData *)m_Operation.pData;
		int res=GetReplyCode();
		if (res==2 || res==3)
		{
			m_pOwner->SetCurrentPath(pData->Current);

			m_Operation.nOpState=MKD_MAKESUBDIRS;
			pData->Current.AddSubdir(pData->Segments.front());
			CString Segment=pData->Segments.front();
			pData->Segments.pop_front();
			if (Send( _T("MKD ") + Segment))
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
				if (!Send(_MPT("CWD ")+pData->Current.GetPath()))
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

			ASSERT(!pData->Segments.empty());

			m_pOwner->SetCurrentPath(pData->Current);

			pData->Current.AddSubdir(pData->Segments.front());
			CString Segment=pData->Segments.front();
			pData->Segments.pop_front();
			if (Send( _T("MKD ") + Segment))
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
			GetReply() == _T("550 Directory already exists"))//Creation was successful, although someone else did the work for us
		{ //Create dir entry in parent dir
			CServerPath path2=pData->Current;
			if (path2.HasParent())
			{
				CString name=path2.GetLastSegment();
				path2=path2.GetParent();

#ifndef MPEXT_NO_CACHE
				CDirectoryCache cache;
				t_directory dir;
				cache.Lock();
				BOOL bCached = TRUE;
				BOOL res = cache.Lookup(path2, m_CurrentServer,dir);
				if (!res)
					bCached = FALSE;
#else
				t_directory dir;
				BOOL res = FALSE;
#endif
				if (!res && m_pDirectoryListing)
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
						LogMessage(__FILE__, __LINE__, this,FZ_LOG_WARNING, _T("Dir already exists in cache!"));
						break;
					}
				if (i==dir.num)
				{
					t_directory::t_direntry *entries=new t_directory::t_direntry[dir.num+1];
					for (i=0;i<dir.num;i++)
						entries[i]=dir.direntry[i];
					entries[i].name=name;
					entries[i].lName=name;
					entries[i].lName.MakeLower();
					entries[i].dir=TRUE;
					entries[i].date.hasdate=FALSE;
					entries[i].size=-1;
					entries[i].bUnsure=FALSE;
					delete [] dir.direntry;
					dir.direntry=entries;
					dir.num++;

#ifndef MPEXT_NO_CACHE
					cache.Store(dir, bCached);
#endif
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

#ifndef MPEXT_NO_CACHE
				cache.Unlock();
#endif
			}
		}

		//Continue operation even if MKD failed, maybe another thread did create this directory for us
		if (pData->Segments.empty())
			ResetOperation(FZ_REPLY_OK);
		else
		{
			if (Send( _T("CWD ") + pData->Current.GetPath()))
				m_Operation.nOpState=MKD_MAKESUBDIRS;
			else
				return;
		}
	}
	else
		ASSERT(FALSE);
}

void CFtpControlSocket::Rename(CString oldName, CString newName, const CServerPath &path, const CServerPath &newPath)
{
	LogMessage(__FILE__, __LINE__, this,FZ_LOG_DEBUG, _T("Rename(\"%s\", \"%s\", \"%s\")  OpMode=%d OpState=%d"), oldName, newName, path.GetPath(),
			   m_Operation.nOpMode, m_Operation.nOpState);
	class CRenameData : public CFtpControlSocket::t_operation::COpData
	{
	public:
		CRenameData() {}
		virtual ~CRenameData() {}
		CString oldName, newName;
		CServerPath path;
		CServerPath newPath;
	};
	if (oldName != _MPT(""))
	{
		ASSERT(newName != _MPT(""));
		ASSERT(!path.IsEmpty());
		ASSERT(m_Operation.nOpMode == CSMODE_NONE);
		ASSERT(m_Operation.nOpState == -1);
		ASSERT(!m_Operation.pData);
		m_Operation.nOpMode = CSMODE_RENAME;
		if (!Send(_MPT("RNFR ") + path.FormatFilename(oldName)))
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
		ASSERT(oldName == _MPT(""));
		ASSERT(path.IsEmpty());
		ASSERT(m_Operation.nOpMode == CSMODE_RENAME);
		ASSERT(m_Operation.pData);
		CRenameData *pData = reinterpret_cast<CRenameData *>(m_Operation.pData);

		if (m_Operation.nOpState == -1)
		{
			int res = GetReplyCode();
			if (res == 2 || res == 3)
			{
				m_Operation.nOpState++;
				if (pData->newPath.IsEmpty())
				{
					if (!Send(_MPT("RNTO ") + pData->path.FormatFilename(((CRenameData *)m_Operation.pData)->newName)))
						return;
				}
				else
					if (!Send(_MPT("RNTO ") + pData->newPath.FormatFilename(((CRenameData *)m_Operation.pData)->newName)))
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

#ifndef MPEXT_NO_CACHE
				CDirectoryCache cache;

				cache.Lock();

				//Rename all references to the directory in the cache
				if (pData->newPath.IsEmpty())
					cache.Rename(pData->path, pData->oldName, pData->newName, m_CurrentServer);

				BOOL bCached = TRUE;
				t_directory dir;
				BOOL res = cache.Lookup(pData->path, m_CurrentServer, dir);
				if (!res)
					bCached = FALSE;
#else
				t_directory dir;
				BOOL res = FALSE;
#endif
				if (!res && m_pDirectoryListing)
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
								dir.direntry[i].lName = pData->newName;
								dir.direntry[i].lName.MakeLower();

#ifndef MPEXT_NO_CACHE
								CDirectoryCache cache;
								cache.Store(dir, bCached);
#endif
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

#ifndef MPEXT_NO_CACHE
								cache.Store(dir, bCached);

								// If directory, delete old directory from cache
								t_directory olddir;
								res = cache.Lookup(pData->path, pData->oldName, m_CurrentServer, olddir);
								if (res)
								{
									cache.Purge(olddir.path, m_CurrentServer);
									t_directory newdir;
									if (cache.Lookup(dir.path, m_CurrentServer, newdir))
										dir = newdir;
								}
#endif

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

#ifndef MPEXT_NO_CACHE
								BOOL bCached = TRUE;
								BOOL res = cache.Lookup(pData->newPath, m_CurrentServer, dir);
								if (!res)
									bCached = FALSE;
#else
								BOOL res = FALSE;
#endif

								if (!res && m_pDirectoryListing)
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
									direntry[dir.num].lName = pData->newName;
									direntry[dir.num].lName.MakeLower();
									dir.num++;
									delete [] dir.direntry;
									dir.direntry = direntry;

#ifndef MPEXT_NO_CACHE
									cache.Store(dir, bCached);
#endif
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
#ifndef MPEXT_NO_CACHE
				cache.Unlock();
#endif
				ResetOperation(FZ_REPLY_OK);
			}
			else
				ResetOperation(FZ_REPLY_ERROR);
		}
	}
}

#ifndef MPEXT_NO_SSL
void CFtpControlSocket::SetVerifyCertResult(int nResult, t_SslCertData *pData)
{
	ASSERT(pData);
	if (!m_pSslLayer)
		return;
	if (!m_Operation.nOpMode == CSMODE_CONNECT)
		return;
	m_bCheckForTimeout = TRUE;
	m_pSslLayer->SetNotifyReply(pData->priv_data, SSL_VERIFY_CERT, nResult);
	m_LastRecvTime = CTime::GetCurrentTime();
}
#endif

void CFtpControlSocket::OnTimer()
{
	CheckForTimeout();
	ResumeTransfer();
	if (COptions::GetOptionVal(OPTION_KEEPALIVE))
	{
		if (!m_pOwner->IsBusy() && m_pOwner->IsConnected() && !m_bKeepAliveActive)
		{
			//Getting intervals for the Keep Alive feature
			int low=COptions::GetOptionVal(OPTION_INTERVALLOW);
			int diff=COptions::GetOptionVal(OPTION_INTERVALHIGH)-low;

			//Choose a new delay
#ifdef MPEXT
			int delay=low+(rand()*diff)/RAND_MAX;
#else
			static int delay=low+(rand()*diff)/RAND_MAX;
#endif

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
	str.Format( _T("SITE CHMOD %03d %s"), nValue, path.FormatFilename(filename));
	Send(str);
}

void CFtpControlSocket::SetAsyncRequestResult(int nAction, CAsyncRequestData *pData)
{
	switch (pData->nRequestType)
	{
	case FZ_ASYNCREQUEST_OVERWRITE:
		SetFileExistsAction(nAction, (COverwriteRequestData *)pData);
		break;
#ifndef MPEXT_NO_SSL
	case FZ_ASYNCREQUEST_VERIFYCERT:
		SetVerifyCertResult(nAction, ((CVerifyCertRequestData *)pData)->pCertData );
		break;
#endif
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
			ShowStatus(IDS_ERRORMSG_INTERRUPTED,1);
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
			ShowStatus(IDS_ERRORMSG_INTERRUPTED,1);
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
			ShowStatus(IDS_ERRORMSG_INTERRUPTED,1);
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
			ShowStatus(IDS_ERRORMSG_INTERRUPTED, 1);
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
		LogMessage(__FILE__, __LINE__, this,FZ_LOG_WARNING, _T("Unknown request reply %d"), pData->nRequestType);
		break;
	}
}

int CFtpControlSocket::OnLayerCallback(std::list<t_callbackMsg>& callbacks)
{
	for (std::list<t_callbackMsg>::iterator iter = callbacks.begin(); iter != callbacks.end(); iter++)
	{
		if (iter->nType == LAYERCALLBACK_STATECHANGE)
		{
#ifndef MPEXT_NO_SSL
			if (iter->pLayer == m_pSslLayer)
			{
				LogMessage(__FILE__, __LINE__, this, FZ_LOG_INFO, _T("m_pSslLayer changed state from %d to %d"), iter->nParam2, iter->nParam1);
				delete [] iter->str;
				continue;
			}
#endif
		}
		else if (iter->nType == LAYERCALLBACK_LAYERSPECIFIC)
		{
#ifndef MPEXT_NO_SSL
			if (iter->pLayer == m_pSslLayer)
			{
				USES_CONVERSION;

				switch (iter->nParam1)
				{
				case SSL_INFO:
					switch (iter->nParam2)
					{
					case SSL_INFO_ESTABLISHED:
						ShowStatus(IDS_STATUSMSG_SSLESTABLISHED, 0);
						PostMessage(m_pOwner->m_hOwnerWnd, m_pOwner->m_nReplyMessageID, FZ_MSG_MAKEMSG(FZ_MSG_SECURESERVER, 1), 0);
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
						ShowStatus(IDS_ERRORMSG_UNKNOWNSSLERROR, 1);
						break;
					case SSL_FAILURE_ESTABLISH:
						ShowStatus(IDS_ERRORMSG_CANTESTABLISHSSLCONNECTION, 1);
						break;
#ifndef MPEXT_NO_SSLDLL
					case SSL_FAILURE_LOADDLLS:
						ShowStatus(IDS_ERRORMSG_CANTLOADSSLDLLS, 1);
						break;
#endif
					case SSL_FAILURE_INITSSL:
						ShowStatus(IDS_ERRORMSG_CANTINITSSL, 1);
						break;
					case SSL_FAILURE_VERIFYCERT:
						ShowStatus(IDS_ERRORMSG_SSLCERTIFICATEERROR, 1);
						break;
					case SSL_FAILURE_CERTREJECTED:
						ShowStatus(IDS_ERRORMSG_CERTREJECTED, 1);
						m_bDidRejectCertificate = TRUE;
						break;
					}
					TriggerEvent(FD_CLOSE);
					break;
				case SSL_VERBOSE_INFO:
					LogMessageRaw(FZ_LOG_INFO, A2CT(iter->str));
					break;
				case SSL_VERBOSE_WARNING:
					LogMessageRaw(FZ_LOG_WARNING, A2CT(iter->str));
					break;
				case SSL_VERIFY_CERT:
					t_SslCertData *pData = new t_SslCertData;
					if (m_pSslLayer->GetPeerCertificateData(*pData))
					{
						CVerifyCertRequestData *pRequestData = new CVerifyCertRequestData;
						pRequestData->nRequestID=m_pOwner->GetNextAsyncRequestID();

						pRequestData->pCertData = pData;

						if (!PostMessage(m_pOwner->m_hOwnerWnd, m_pOwner->m_nReplyMessageID, FZ_MSG_MAKEMSG(FZ_MSG_ASYNCREQUEST, FZ_ASYNCREQUEST_VERIFYCERT), (LPARAM)pRequestData))
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
#endif
#endif
#ifndef MPEXT_NO_GSS
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
		std::list<t_callbackMsg> tmp;
		tmp.push_back(*iter);
		CControlSocket::OnLayerCallback(tmp);
	}
	return 0;
}

BOOL CFtpControlSocket::ParsePwdReply(CString& rawpwd)
{
	CListData *pData = static_cast<CListData *>(m_Operation.pData);
	ASSERT(pData);

	int pos1 = rawpwd.Find(_MPT('"'));
	int pos2 = rawpwd.ReverseFind(_MPT('"'));
	if (pos1 == -1 || pos2 == -1 || pos1 >= pos2)
	{
		LogMessage(__FILE__, __LINE__, this, FZ_LOG_WARNING, _T("No quoted path found, try using first token as path"));
		pos1 = rawpwd.Find(_MPT(' '));
		if (pos1 != -1)
		{
			pos2 = rawpwd.Find(_MPT(' '), pos1 + 1);
			if (pos2 == -1)
				pos2 = rawpwd.GetLength();
		}

		if (pos1 == -1)
		{
			LogMessage(__FILE__, __LINE__, this, FZ_LOG_WARNING, _T("Can't parse path!"));
			ResetOperation(FZ_REPLY_ERROR);
			return FALSE;
		}
	}
	rawpwd = rawpwd.Mid(pos1 + 1, pos2 - pos1 - 1);

	CServerPath realPath = m_pOwner->GetCurrentPath();
	realPath.SetServer(m_CurrentServer);
	if (!realPath.SetPath(rawpwd))
	{
		LogMessage(__FILE__, __LINE__, this, FZ_LOG_WARNING, _T("Can't parse path!"));
		ResetOperation(FZ_REPLY_ERROR);
		return FALSE;
	}
	m_pOwner->SetCurrentPath(realPath);

	return TRUE;
}

void CFtpControlSocket::DiscardLine(CStringA line)
{
	if (m_Operation.nOpMode == CSMODE_CONNECT && m_Operation.nOpState == CONNECT_FEAT)
	{
		line.MakeUpper();
#ifndef MPEXT_NO_ZLIB
		if (line == _MPAT(" MODE Z") || line.Left(8) == _MPAT(" MODE Z "))
			m_zlibSupported = true;
		else
#endif
			if (line == _MPAT(" UTF8") && m_CurrentServer.nUTF8 != 2)
			m_bAnnouncesUTF8 = true;
		else if (line == _MPAT(" CLNT") || line.Left(6) == _MPAT(" CLNT "))
			m_hasClntCmd = true;
#ifdef MPEXT
		else if (line == _MPAT(" MLSD"))
		{
			m_serverCapabilities.SetCapability(mlsd_command, yes);
		}
		else if (line.Left(5) == _MPAT(" MLST"))
		{
			USES_CONVERSION;
			m_serverCapabilities.SetCapability(mlsd_command, yes, (LPCSTR)line.Mid(6, -1));
		}
		else if (line == _MPAT(" MFMT"))
		{
			m_serverCapabilities.SetCapability(mfmt_command, yes);
		}
#endif
	}
#ifdef MPEXT
	else if (m_Operation.nOpMode == CSMODE_LISTFILE)
	{
		m_ListFile = line;
	}
#endif
}

bool CFtpControlSocket::NeedModeCommand()
{
#ifdef MPEXT_NO_ZLIB
	return false;
#else
	bool useZlib;
	if (m_Operation.nOpMode == CSMODE_LIST || (m_Operation.nOpMode == CSMODE_TRANSFER && m_Operation.nOpMode <= FILETRANSFER_TYPE))
		useZlib = COptions::GetOptionVal(OPTION_MODEZ_USE) != 0;
	else
		useZlib = COptions::GetOptionVal(OPTION_MODEZ_USE) > 1;

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
	return m_zlibLevel != COptions::GetOptionVal(OPTION_MODEZ_LEVEL);
#endif
}

CString CFtpControlSocket::GetReply()
{
	if (m_RecvBuffer.empty())
		return _MPT("");

	USES_CONVERSION;

	LPCSTR line = (LPCSTR)m_RecvBuffer.front();
	if (m_bUTF8)
	{
		// convert from UTF-8 to ANSI
		LPCSTR utf8 = (LPCSTR)m_RecvBuffer.front();
		if (m_Operation.nOpMode&CSMODE_LISTFILE && m_Operation.nOpState==LIST_LISTFILE)
		{
			if (GetReplyCode() == 2)
				line = (LPCSTR)m_ListFile;
		}
		if (!utf8_valid((const unsigned char*)line, strlen(line)))
		{
			if (m_CurrentServer.nUTF8 != 1)
			{
				LogMessage(__FILE__, __LINE__, this, FZ_LOG_WARNING, _T("Server does not send proper UTF-8, falling back to local charset"));
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
			return _MPT("");
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
		if (GetLastError() != WSAEWOULDBLOCK)
		{
			ShowStatus(IDS_ERRORMSG_CANTSENDCOMMAND, 1);
			DoClose();
		}
		return;
	}
	if (!res)
	{
		ShowStatus(IDS_ERRORMSG_CANTSENDCOMMAND, 1);
		DoClose();
	}

	m_awaitsReply = true;
	m_LastSendTime = CTime::GetCurrentTime();
	PostMessage(m_pOwner->m_hOwnerWnd, m_pOwner->m_nReplyMessageID, FZ_MSG_MAKEMSG(FZ_MSG_SOCKETSTATUS, FZ_SOCKETSTATUS_SEND), 0);

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
	if (!retmsg.CompareNoCase(_T("550 No members found.")))
		return true;

	if (!retmsg.CompareNoCase(_T("550 No data sets found.")))
		return true;

	if (!retmsg.CompareNoCase(_T("550 No files found.")))
		return true;

	return false;
}

#ifdef MPEXT
bool CFtpControlSocket::IsRoutableAddress(const CString & host)
{
	USES_CONVERSION;

	if (host.Left(3) == _T("127") ||
	    host.Left(3) == _T("10.") ||
	    host.Left(7) == _T("192.168") ||
	    host.Left(7) == _T("169.254"))
	{
		return false;
	}
	else if (host.Left(3) == _T("172"))
	{
		CString middle = host.Mid(4);
		int pos = middle.Find(_T("."));
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
				LogMessage(__FILE__, __LINE__, this, FZ_LOG_WARNING, _T("Error retrieving server address"));
				result = false;
			}
			else if (ahost != host)
			{
				LogMessage(__FILE__, __LINE__, this, FZ_LOG_WARNING, _T("Using host address %s instead of the one suggested by the server: %s"), ahost, host);
				host = ahost;
			}
			break;

		case 1: // off
			// noop
			break;

		default: // auto
			if (!GetPeerName(ahost, tmpPort))
			{
			    // this is not failure in "auto" mode
				LogMessage(__FILE__, __LINE__, this, FZ_LOG_WARNING, _T("Error retrieving server address, cannot test if address is routable"));
			}
			else if (!IsRoutableAddress(host) && IsRoutableAddress(ahost))
			{
				LogMessage(__FILE__, __LINE__, this, FZ_LOG_WARNING, _T("Server sent passive reply with unroutable address %s, using host address instead."), host, ahost);
				host = ahost;
			}
			break;
	}

	return result;
}
#endif

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

