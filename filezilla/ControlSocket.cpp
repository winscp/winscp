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

// ControlSocket.cpp: Implementierung der Klasse CControlSocket.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "ControlSocket.h"
#include "mainthread.h"
#include "AsyncProxySocketLayer.h"
#ifndef MPEXT_NO_SSL
#include "AsyncSslSocketLayer.h"
#endif
#ifndef MPEXT_NO_GSS
#include "AsyncGssSocketLayer.h"
#endif
#ifndef MPEXT_NO_SPEED_LIM
#include "SpeedLimit.h"
#endif
#ifndef MPEXT
#include <idna.h>
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

#ifndef MPEXT_NO_SSL
std::list<CControlSocket::t_ActiveList> CControlSocket::m_InstanceList[2];
#else
// explicit initialization prevents an assertion in borland's compiler
std::list<CControlSocket::t_ActiveList> CControlSocket::m_InstanceList[2] =
  {std::list<CControlSocket::t_ActiveList>(), std::list<CControlSocket::t_ActiveList>()};
#endif

CTime CControlSocket::m_CurrentTransferTime[2] = { CTime::GetCurrentTime(), CTime::GetCurrentTime() };
_int64 CControlSocket::m_CurrentTransferLimit[2] = {0, 0};

CCriticalSection CControlSocket::m_SpeedLimitSync;


//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

CControlSocket::CControlSocket(CMainThread *pMainThread)
{
	ASSERT(pMainThread);
	m_pOwner=pMainThread;

	m_Operation.nOpMode=0;
	m_Operation.nOpState=-1;
	m_Operation.pData=0;

	m_pProxyLayer = NULL;
#ifndef MPEXT_NO_SSL
	m_pSslLayer = NULL;
#endif
#ifndef MPEXT_NO_GSS
	m_pGssLayer = NULL;
#endif

	m_pDirectoryListing=0;
#ifndef MPEXT_NO_IDENT
	m_pIdentControl=0;
#endif
}

CControlSocket::~CControlSocket()
{
	LogMessage(__FILE__, __LINE__, this, FZ_LOG_DEBUG, _T("~CControlSocket()"));
	Close();
}

/////////////////////////////////////////////////////////////////////////////
// Member-Funktion CControlSocket 
#define CONNECT_INIT -1
#ifndef MPEXT_NO_GSS
#define CONNECT_GSS -3
#endif
#ifndef MPEXT_NO_SSL
#define CONNECT_SSL_INIT -6
#define CONNECT_SSL_NEGOTIATE -5
#define CONNECT_SSL_WAITDONE -4 
#endif

void CControlSocket::ShowStatus(UINT nID, int type) const
{
	CString str;
	str.LoadString(nID);
	ShowStatus(str, type);
}

void CControlSocket::ShowStatus(CString status, int type) const
{
	if ( status.Left(5)==_T("PASS ") )
	{
		int len=status.GetLength()-5;
		status=_T("PASS ");
		for (int i=0;i<len;i++)
			status+="*";
	}
	else if ( status.Left(5)==_T("ACCT ") )
	{
		int len=status.GetLength()-5;
		status=_T("ACCT ");
		for (int i=0;i<len;i++)
			status+="*";
	}
	LogMessageRaw(type, (LPCTSTR)status);
}


t_server CControlSocket::GetCurrentServer()
{
	return m_CurrentServer;
}

void CControlSocket::Close()
{
#ifndef MPEXT_NO_IDENT
	if(m_pIdentControl)
		delete m_pIdentControl;
	m_pIdentControl=0;
#endif

	if (m_pDirectoryListing)
	{
		delete m_pDirectoryListing;
	}
	m_pDirectoryListing=0;
	CAsyncSocketEx::Close();

	delete m_pProxyLayer;
	m_pProxyLayer = NULL;

#ifndef MPEXT_NO_SSL
	delete m_pSslLayer;
	m_pSslLayer = NULL;
#endif

#ifndef MPEXT_NO_GSS
	delete m_pGssLayer;
	m_pGssLayer = NULL;
#endif

	RemoveActiveTransfer();
}

BOOL CControlSocket::Connect(CString hostAddress, UINT nHostPort)
{
	hostAddress = ConvertDomainName(hostAddress);

	//Don't resolve host asynchronously when using proxies
	if (m_pProxyLayer)
	{
		//If using proxies, we can't use ident -> won't be reachable from outside
		
		return CAsyncSocketEx::Connect(hostAddress, nHostPort);
	}
	BOOL res = CAsyncSocketEx::Connect(hostAddress, nHostPort);
	int nLastError = WSAGetLastError();
	if (res || nLastError==WSAEWOULDBLOCK)
	{
#ifndef MPEXT_NO_IDENT
		if (COptions::GetOptionVal(OPTION_IDENT))
			m_pIdentControl = new CIdentServerControl(this);
#endif
		WSASetLastError(nLastError);
	}
	
	return res;
}

void CControlSocket::SetDirectoryListing(t_directory *pDirectory, bool bSetWorkingDir /*=true*/)
{
	if (m_pDirectoryListing)
		delete m_pDirectoryListing;
	m_CurrentServer=pDirectory->server;
	m_pDirectoryListing=new t_directory;
	*m_pDirectoryListing=*pDirectory;

	if (bSetWorkingDir)
		m_pOwner->SetWorkingDir(pDirectory);
}

int CControlSocket::OnLayerCallback(std::list<t_callbackMsg>& callbacks)
{
	USES_CONVERSION;

	for (std::list<t_callbackMsg>::iterator iter = callbacks.begin(); iter != callbacks.end(); iter++)
	{
		if (iter->nType == LAYERCALLBACK_STATECHANGE)
		{
			if (iter->pLayer == m_pProxyLayer)
				LogMessage(__FILE__, __LINE__, this, FZ_LOG_INFO, _T("m_pProxyLayer changed state from %d to %d"), iter->nParam2, iter->nParam1);
#ifndef MPEXT_NO_GSS
			else if (iter->pLayer == m_pGssLayer)
				LogMessage(__FILE__, __LINE__, this, FZ_LOG_INFO, _T("m_pGssLayer changed state from %d to %d"), iter->nParam2, iter->nParam1);
#endif
			else
				LogMessage(__FILE__, __LINE__, this, FZ_LOG_INFO, _T("Layer @ %d changed state from %d to %d"), iter->pLayer, iter->nParam2, iter->nParam1);
		}
		else if (iter->nType == LAYERCALLBACK_LAYERSPECIFIC)
		{
			if (iter->pLayer == m_pProxyLayer)
			{
				switch (iter->nParam1)
				{
				case PROXYERROR_NOCONN:
					ShowStatus(IDS_ERRORMSG_PROXY_NOCONN, 1);
					break;
				case PROXYERROR_REQUESTFAILED:
					ShowStatus(IDS_ERRORMSG_PROXY_REQUESTFAILED, 1);
					if (iter->str)
						ShowStatus(A2T(iter->str), 1);
					break;
				case PROXYERROR_AUTHTYPEUNKNOWN:
					ShowStatus(IDS_ERRORMSG_PROXY_AUTHTYPEUNKNOWN, 1);
					break;
				case PROXYERROR_AUTHFAILED:
					ShowStatus(IDS_ERRORMSG_PROXY_AUTHFAILED, 1);
					break;
				case PROXYERROR_AUTHNOLOGON:
					ShowStatus(IDS_ERRORMSG_PROXY_AUTHNOLOGON, 1);
					break;
				case PROXYERROR_CANTRESOLVEHOST:
					ShowStatus(IDS_ERRORMSG_PROXY_CANTRESOLVEHOST, 1);
					break;
				default:
					LogMessage(__FILE__, __LINE__, this, FZ_LOG_WARNING, _T("Unknown proxy error") );
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
					ShowStatus(A2CT(iter->str), 2);
					break;
				case GSS_REPLY:
					ShowStatus(A2CT(iter->str), 3);
					break;
				}
			}
#endif
		}

		delete [] iter->str;
	}

	return 1;
}

#ifndef MPEXT_NO_SPEED_LIM
_int64 CControlSocket::GetSpeedLimit(CTime &time, int valType, int valValue, SPEEDLIMITSLIST &list)
{
	int type = COptions::GetOptionVal(valType);

	if ( type == 1)
		return ( _int64)COptions::GetOptionVal(valValue) * 1024;

	if ( type == 2)
	{
		CSingleLock lock(&COptions::m_Sync, TRUE);
		for ( unsigned int i = 0; i < list.size(); i++)
		{
			if ( list[ i]->IsItActive(time) && list[i]->m_Speed)
				return list[ i]->m_Speed * 1024;
		}
	}

	return ( _int64)1000000000000;	//I hope that when there will be something with 1000GB/s then I'll change it :)
}
#endif

_int64 CControlSocket::GetSpeedLimit(enum transferDirection direction, CTime &time)
{
#ifndef MPEXT_NO_SPEED_LIM
	if (direction == download)
		return GetSpeedLimit(time, OPTION_SPEEDLIMIT_DOWNLOAD_TYPE, OPTION_SPEEDLIMIT_DOWNLOAD_VALUE, COptions::m_DownloadSpeedLimits);
	else
		return GetSpeedLimit( time, OPTION_SPEEDLIMIT_UPLOAD_TYPE, OPTION_SPEEDLIMIT_UPLOAD_VALUE, COptions::m_UploadSpeedLimits);
#else
	return ( _int64)1000000000000;
#endif
}

_int64 CControlSocket::GetAbleToUDSize( bool &beenWaiting, CTime &curTime, _int64 &curLimit, std::list<CControlSocket::t_ActiveList>::iterator &iter, enum transferDirection direction, int nBufSize)
{
	beenWaiting = false;

	CTime nowTime = CTime::GetCurrentTime();
	_int64 ableToRead = BUFSIZE;

	if ( nowTime == curTime)
	{
		ableToRead = iter->nBytesAvailable;

		if (ableToRead <= 0)
		{
			//	we should wait till next second
			nowTime = CTime::GetCurrentTime();

			while (nowTime == curTime && !iter->nBytesAvailable)
			{
				if (beenWaiting)
				{
					//Check if there are other commands in the command queue.
					MSG msg;
					if (PeekMessage(&msg, 0, m_pOwner->m_nInternalMessageID, m_pOwner->m_nInternalMessageID, PM_NOREMOVE))
					{
						LogMessage(__FILE__, __LINE__, this, FZ_LOG_INFO, _T("Message waiting in queue, resuming later"));
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

_int64 CControlSocket::GetAbleToTransferSize(enum transferDirection direction, bool &beenWaiting, int nBufSize)
{
	m_SpeedLimitSync.Lock();
	std::list<t_ActiveList>::iterator iter;
	for (iter = m_InstanceList[direction].begin(); iter != m_InstanceList[direction].end(); iter++)
		if (iter->pOwner == this)
			break;
	if (iter == m_InstanceList[direction].end())
	{
		t_ActiveList item;
#ifdef MPEXT
		CTime time = CTime::GetCurrentTime();
		item.nBytesAvailable = GetSpeedLimit(direction, time) / (m_InstanceList[direction].size() + 1);
#else
		item.nBytesAvailable = GetSpeedLimit(direction, CTime::GetCurrentTime()) / (m_InstanceList[direction].size() + 1);
#endif
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

BOOL CControlSocket::RemoveActiveTransfer()
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

BOOL CControlSocket::SpeedLimitAddTransferredBytes(enum transferDirection direction, _int64 nBytesTransferred)
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

CString CControlSocket::ConvertDomainName(CString domain)
{
	USES_CONVERSION;

	LPCWSTR buffer = T2CW(domain);
	
	char *utf8 = new char[wcslen(buffer) * 2 + 2];
	if (!WideCharToMultiByte(CP_UTF8, 0, buffer, -1, utf8, wcslen(buffer) * 2 + 2, 0, 0))
	{
		delete [] utf8;
		LogMessage(FZ_LOG_WARNING, _T("Could not convert domain name"));
		return domain;
	}

	char *output = 0;
#ifdef MPEXT
	output = strdup(utf8);
#else
	if (idna_to_ascii_8z(utf8, &output, IDNA_ALLOW_UNASSIGNED))
	{
		delete [] utf8;
		LogMessage(FZ_LOG_WARNING, _T("Could not convert domain name"));
		return domain;
	}
#endif
	delete [] utf8;

	CString result = A2T(output);
	free(output);
	return result;
}