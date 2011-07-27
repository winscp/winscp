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

// TransferSocket.cpp: Implementierungsdatei
//

#include "stdafx.h"
#ifndef MPEXT
#include "filezilla.h"
#endif
#include "TransferSocket.h"
#include "mainthread.h"
#include "AsyncProxySocketLayer.h"
#ifndef MPEXT_NO_GSS
#include "AsyncGssSocketLayer.h"
#endif

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define BUFSIZE 16384

#define STATE_WAITING		0
#define STATE_STARTING		1
#define STATE_STARTED		2

/////////////////////////////////////////////////////////////////////////////
// CTransferSocket

CTransferSocket::CTransferSocket(CFtpControlSocket *pOwner, int nMode)
{
	ASSERT(pOwner);
	InitLog(pOwner);
	m_pOwner = pOwner;
	m_nMode = nMode;
	m_nTransferState = STATE_WAITING;
	m_bCheckTimeout = FALSE;
	m_pBuffer = 0;
#ifndef MPEXT_NO_ZLIB
	m_pBuffer2 = 0;
#endif
	m_bufferpos = 0;
	m_ReadPos = 0;
	m_ReadBuffer = 0;
	m_ReadSize = 0;
	m_cLastChar = 0;
	m_pFile = 0;
	m_bListening = FALSE;
	m_bSentClose = FALSE;
	m_nInternalMessageID = 0;
	m_transferdata.transfersize = 0;
	m_transferdata.transferleft = 0;
	m_transferdata.nTransferStart = 0;
	m_nNotifyWaiting = 0;
	m_bShutDown = FALSE;

	UpdateStatusBar(true);
	
	for (int i = 0; i < SPEED_SECONDS; i++)
	{
		m_Transfered[i] = 0;
		m_UsedForTransfer[i] = 0;
	}

	m_pProxyLayer = NULL;
#ifndef MPEXT_NO_SSL
	m_pSslLayer = NULL;
#endif
#ifndef MPEXT_NO_GSS
	m_pGssLayer = NULL;
#endif

	if (m_nMode & CSMODE_LIST)
	{
		m_pListResult = new CFtpListResult(pOwner->m_CurrentServer, &pOwner->m_bUTF8);
		m_pListResult->InitLog(this);
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
	LogMessage(__FILE__, __LINE__, this,FZ_LOG_DEBUG, _T("~CTransferSocket()"));
	delete [] m_pBuffer;
#ifndef MPEXT_NO_ZLIB
	delete [] m_pBuffer2;
#endif
	if (m_ReadBuffer)
	{
		delete [] m_ReadBuffer;
		m_ReadBuffer = 0;
	}
	PostMessage(m_pOwner->m_pOwner->m_hOwnerWnd, m_pOwner->m_pOwner->m_nReplyMessageID, FZ_MSG_MAKEMSG(FZ_MSG_TRANSFERSTATUS, 0), 0);
	Close();
	RemoveAllLayers();
	delete m_pProxyLayer;
#ifndef MPEXT_NO_SSL
	delete m_pSslLayer;
#endif
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
		
		char *buffer = new char[BUFSIZE];
		int numread = CAsyncSocketEx::Receive(buffer, BUFSIZE);
		if (numread != SOCKET_ERROR && numread)
		{
			m_LastActiveTime = CTime::GetCurrentTime();
			UpdateRecvLed();

#ifndef MPEXT_NO_ZLIB
			if (m_useZlib)
			{
				m_zlibStream.next_in = (Bytef *)buffer;
				m_zlibStream.avail_in = numread;
				char *out = new char[BUFSIZE];
				m_zlibStream.next_out = (Bytef *)out;
				m_zlibStream.avail_out = BUFSIZE;
				int res = inflate(&m_zlibStream, 0);
				while (res == Z_OK)
				{
					m_pListResult->AddData(out, BUFSIZE - m_zlibStream.avail_out);
					out = new char[BUFSIZE];
					m_zlibStream.next_out = (Bytef *)out;
					m_zlibStream.avail_out = BUFSIZE;
					res = inflate(&m_zlibStream, 0);
				}
				delete [] buffer;
				if (res == Z_STREAM_END)
					m_pListResult->AddData(out, BUFSIZE - m_zlibStream.avail_out);
				else if (res != Z_OK && res != Z_BUF_ERROR)
				{
					delete [] out;
					Close();
					if (!m_bSentClose)
					{
						m_nMode |= CSMODE_TRANSFERERROR;
						m_bSentClose = TRUE;
						m_pOwner->m_pOwner->PostThreadMessage(m_nInternalMessageID, FZAPI_THREADMSG_TRANSFEREND, m_nMode);
					}
					return;
				}
				else
					delete [] out;
			}
			else
#endif
				m_pListResult->AddData(buffer, numread);
			m_transferdata.transfersize += numread;
			CTimeSpan timespan = CTime::GetCurrentTime() - m_StartTime;
			int elapsed = (int)timespan.GetTotalSeconds();
			//TODO
			//There are servers which report the total number of 
			//bytes in the list response message, but yet it is not supported by FZ.
			/*double leftmodifier=(transfersize-transferstart-transferleft); 
			leftmodifier*=100;
			leftmodifier/=(transfersize-transferstart);
			if (leftmodifier==0)
				leftmodifier=1;
			double leftmodifier2=100-leftmodifier;
			int left=(int)((elapsed/leftmodifier)*leftmodifier2);
			int percent=MulDiv(100,transfersize-transferleft,transfersize);*/
			int transferrate=static_cast<int>( (elapsed && m_transferdata.transfersize)?m_transferdata.transfersize/elapsed:0 );
			t_ffam_transferstatus *status = new t_ffam_transferstatus;
			status->bFileTransfer = FALSE;
#ifdef MPEXT
			status->transfersize = -1;
#endif
			status->bytes = m_transferdata.transfersize;
			status->percent = -1;
			status->timeelapsed = elapsed;
			status->timeleft = -1;
			status->transferrate = transferrate;
			PostMessage(m_pOwner->m_pOwner->m_hOwnerWnd, m_pOwner->m_pOwner->m_nReplyMessageID, FZ_MSG_MAKEMSG(FZ_MSG_TRANSFERSTATUS, 0), (LPARAM)status);
		}
		else
			delete [] buffer;
		if (!numread)
		{
			Close();
			if (!m_bSentClose)
			{
				m_bSentClose = TRUE;
				m_pOwner->m_pOwner->PostThreadMessage(m_nInternalMessageID, FZAPI_THREADMSG_TRANSFEREND, m_nMode);
			}
		}
		if (numread == SOCKET_ERROR)
		{
			int nError = GetLastError();
			if (nError == WSAENOTCONN)
			{
				//Not yet connected
				return;
			}
#ifndef MPEXT_NO_SSL
			else if (m_pSslLayer && nError == WSAESHUTDOWN)
			{
				// Do nothing, wait for shutdown complete notification.
				return;
			}
#endif
			else if (nError != WSAEWOULDBLOCK)
			{
				Close();
				if (!m_bSentClose)
				{
					m_nMode |= CSMODE_TRANSFERERROR;
					m_bSentClose = TRUE;
					m_pOwner->m_pOwner->PostThreadMessage(m_nInternalMessageID, FZAPI_THREADMSG_TRANSFEREND, m_nMode);
				}
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
			ableToRead = m_pOwner->GetAbleToTransferSize(CControlSocket::download, beenWaiting);
		else
			ableToRead = BUFSIZE;

		if (!beenWaiting)
			ASSERT(ableToRead);
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
			Transfered( numread, CTime::GetCurrentTime());
			m_pOwner->SpeedLimitAddTransferredBytes(CControlSocket::download, numread);
		}

		if (!numread)
		{
			Close();
			if (!m_bSentClose)
			{
				m_bSentClose = TRUE;
				m_pOwner->m_pOwner->PostThreadMessage(m_nInternalMessageID, FZAPI_THREADMSG_TRANSFEREND, m_nMode);
			}
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
#ifndef MPEXT_NO_SSL
			else if (m_pSslLayer && nError == WSAESHUTDOWN)
			{
				// Do nothing, wait for shutdown complete notification.
				return;
			}
#endif
			else if (nError != WSAEWOULDBLOCK)
			{
				Close();
				if (!m_bSentClose)
				{
					m_nMode |= CSMODE_TRANSFERERROR;
					m_bSentClose = TRUE;
					m_pOwner->m_pOwner->PostThreadMessage(m_nInternalMessageID, FZAPI_THREADMSG_TRANSFEREND, m_nMode);
				}
			}

			UpdateStatusBar(false);
			return;
		}
		
		int written = 0;
		m_LastActiveTime = CTime::GetCurrentTime();
		UpdateRecvLed();
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
					m_pFile->Write(m_pBuffer2, BUFSIZE - m_zlibStream.avail_out);
					written += BUFSIZE - m_zlibStream.avail_out;
					m_zlibStream.next_out = (Bytef *)m_pBuffer2;
					m_zlibStream.avail_out = BUFSIZE;
					res = inflate(&m_zlibStream, 0);
				}
				if (res == Z_STREAM_END)
				{
					m_pFile->Write(m_pBuffer2, BUFSIZE - m_zlibStream.avail_out);
					written += BUFSIZE - m_zlibStream.avail_out;
				}
				else if (res != Z_OK && res != Z_BUF_ERROR)
				{
					Close();
					if (!m_bSentClose)
					{
						m_nMode |= CSMODE_TRANSFERERROR;
						m_bSentClose = TRUE;
						m_pOwner->m_pOwner->PostThreadMessage(m_nInternalMessageID, FZAPI_THREADMSG_TRANSFEREND, m_nMode);
					}
					return;
				}
			}
			else
#endif
			{
				m_pFile->Write(m_pBuffer, numread);
				written = numread;
			}
		}
		CATCH(CFileException,e)
		{
			LPTSTR msg = new TCHAR[BUFSIZE];
			if (e->GetErrorMessage(msg, BUFSIZE))
				m_pOwner->ShowStatus(msg, 1);
			delete [] msg;
			Close();
			if (!m_bSentClose)
			{
				m_nMode |= CSMODE_TRANSFERERROR;
				m_bSentClose = TRUE;
				m_pOwner->m_pOwner->PostThreadMessage(m_nInternalMessageID, FZAPI_THREADMSG_TRANSFEREND, m_nMode);
			}
			return;
		}
		END_CATCH;
		m_transferdata.transferleft -= written;

		UpdateStatusBar(false);
	}
}

void CTransferSocket::OnAccept(int nErrorCode) 
{
	LogMessage(__FILE__, __LINE__, this,FZ_LOG_DEBUG, _T("OnAccept(%d)"), nErrorCode);
	m_bListening=FALSE;
	CAsyncSocketEx tmp;
	Accept(tmp);
	SOCKET socket=tmp.Detach();
	CAsyncSocketEx::Close();
	
	Attach(socket);

	/* Set internal socket send buffer to twice the programs buffer size
	 * this should fix the speed problems some users have reported
	 */
	DWORD value;
	int len = sizeof(value);
	GetSockOpt(SO_SNDBUF, &value, &len);
	if (value < (BUFSIZE*2))
	{
		value = BUFSIZE * 2;
		SetSockOpt(SO_SNDBUF, &value, sizeof(value));
	}

	if (m_nTransferState == STATE_STARTING)
	{
		m_nTransferState = STATE_STARTED;	
		
#ifndef MPEXT_NO_SSL
		if (m_pSslLayer)
		{
			AddLayer(m_pSslLayer);
			int res = m_pSslLayer->InitSSLConnection(true);
#ifndef MPEXT_NO_SSLDLL
			if (res == SSL_FAILURE_LOADDLLS)
				m_pOwner->ShowStatus(IDS_ERRORMSG_CANTLOADSSLDLLS, 1);
			else
#endif
			if (res == SSL_FAILURE_INITSSL)
				m_pOwner->ShowStatus(IDS_ERRORMSG_CANTINITSSL, 1);
					
			if (res)
			{
				Close();
				if (!m_bSentClose)
				{
					m_nMode |= CSMODE_TRANSFERERROR;
					m_bSentClose = TRUE;
					m_pOwner->m_pOwner->PostThreadMessage(m_nInternalMessageID, FZAPI_THREADMSG_TRANSFEREND, m_nMode);
				}
				return;
			}
		}
#endif
		
#ifndef MPEXT_NO_GSS
		if (m_pGssLayer)
		{
			AddLayer(m_pGssLayer);
		}
#endif
		
		m_TransferedFirst = m_StartTime = CTime::GetCurrentTime();
		m_LastActiveTime = CTime::GetCurrentTime();
	}
}

void CTransferSocket::OnConnect(int nErrorCode) 
{
	LogMessage(__FILE__, __LINE__, this,FZ_LOG_DEBUG, _T("OnConnect(%d)"), nErrorCode);
	if (nErrorCode)
	{
		TCHAR buffer[1000];
		memset(buffer,0,1000);
		FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, 0, nErrorCode, 0, buffer, 999, 0);
		CString str;
		str.Format(IDS_ERRORMSG_CANTOPENTRANSFERCHANNEL,buffer);
		str.Replace( _T("\n"), _T("\0") );
		str.Replace( _T("\r"), _T("\0") );
		m_pOwner->ShowStatus(str, 1);
		Close();
		if (!m_bSentClose)
		{
			m_nMode|=CSMODE_TRANSFERERROR;
			m_bSentClose=TRUE;
			m_pOwner->m_pOwner->PostThreadMessage(m_nInternalMessageID, FZAPI_THREADMSG_TRANSFEREND, m_nMode);
		}	
	}
	else
	{
		/* Set internal socket send buffer to twice the programs buffer size
		 * this should fix the speed problems some users have reported
		 */
		DWORD value;
		int len = sizeof(value);
		GetSockOpt(SO_SNDBUF, &value, &len);
		if (value < (BUFSIZE*2))
		{
			value = BUFSIZE * 2;
			SetSockOpt(SO_SNDBUF, &value, sizeof(value));
		}
	}
	if (m_nTransferState == STATE_STARTING)
	{
		m_nTransferState = STATE_STARTED;
		
		m_TransferedFirst = m_StartTime = CTime::GetCurrentTime();
		m_LastActiveTime=CTime::GetCurrentTime();
		
#ifndef MPEXT_NO_SSL
		if (m_pSslLayer)
		{
			AddLayer(m_pSslLayer);
			int res = m_pSslLayer->InitSSLConnection(true);
#ifndef MPEXT_NO_SSLDLL
			if (res == SSL_FAILURE_LOADDLLS)
				m_pOwner->ShowStatus(IDS_ERRORMSG_CANTLOADSSLDLLS, 1);
			else
#endif
			if (res == SSL_FAILURE_INITSSL)
				m_pOwner->ShowStatus(IDS_ERRORMSG_CANTINITSSL, 1);
					
			if (res)
			{
				Close();
				if (!m_bSentClose)
				{
					m_nMode |= CSMODE_TRANSFERERROR;
					m_bSentClose = TRUE;
					m_pOwner->m_pOwner->PostThreadMessage(m_nInternalMessageID, FZAPI_THREADMSG_TRANSFEREND, m_nMode);
				}
				return;
			}
		}
#endif
		
#ifndef MPEXT_NO_GSS
		if (m_pGssLayer)
		{
			AddLayer(m_pGssLayer);
		}
#endif
	}
}	


void CTransferSocket::OnClose(int nErrorCode) 
{
	LogMessage(__FILE__, __LINE__, this, FZ_LOG_DEBUG, _T("OnClose(%d)"), nErrorCode);

	if (m_nTransferState == STATE_WAITING)
	{
		m_nNotifyWaiting |= FD_CLOSE;
		return;
	}

	OnReceive(0);
	Close();
	if (!m_bSentClose)
	{
		m_bSentClose=TRUE;
		m_pOwner->m_pOwner->PostThreadMessage(m_nInternalMessageID, FZAPI_THREADMSG_TRANSFEREND, m_nMode);
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
		m_pOwner->ShowStatus(IDS_ERRORMSG_TIMEOUT, 1);
		Close();
		if (!m_bSentClose)
		{
			m_nMode |= CSMODE_TRANSFERTIMEOUT;
			m_bSentClose = TRUE;
			VERIFY(m_pOwner->m_pOwner->PostThreadMessage(m_nInternalMessageID, FZAPI_THREADMSG_TRANSFEREND, m_nMode));
		}
		return 2;
	}
	return 1;
}

void CTransferSocket::SetActive()
{
	LogMessage(__FILE__, __LINE__, this, FZ_LOG_DEBUG, _T("SetActive()"));

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
					m_pOwner->ShowStatus("ZLib error", 1);
					Close();
					if (!m_bSentClose)
					{
						m_nMode |= CSMODE_TRANSFERERROR;
						m_bSentClose = TRUE;
						m_pOwner->m_pOwner->PostThreadMessage(m_nInternalMessageID, FZAPI_THREADMSG_TRANSFEREND, m_nMode);
					}
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

			int nLimit = (int)m_pOwner->GetAbleToTransferSize(CControlSocket::upload, beenWaiting);
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
#ifndef MPEXT_NO_SSL
				else if (m_pSslLayer && nError == WSAESHUTDOWN)
				{
					// Do nothing, wait for shutdown complete notification.
					return;
				}
#endif
				else if (nError != WSAEWOULDBLOCK)
				{
					if (ShutDown() || GetLastError() != WSAEWOULDBLOCK)
					{
						Close();
						if (!m_bSentClose)
						{
							m_nMode |= CSMODE_TRANSFERERROR;
							m_bSentClose = TRUE;
							m_pOwner->m_pOwner->PostThreadMessage(m_nInternalMessageID, FZAPI_THREADMSG_TRANSFEREND, m_nMode);
						}
					}
				}
				UpdateStatusBar(false);
				return;
			}

			Transfered( numsent, CTime::GetCurrentTime());
			m_pOwner->SpeedLimitAddTransferredBytes(CControlSocket::upload, numsent);
			m_LastActiveTime = CTime::GetCurrentTime();
			UpdateSendLed();

			m_bufferpos += numsent;

			UpdateStatusBar(false);
			
			if (!m_zlibStream.avail_in && !m_pFile && m_zlibStream.avail_out &&
				m_zlibStream.avail_out + m_bufferpos == BUFSIZE && res == Z_STREAM_END)
			{
				if (ShutDown() || GetLastError() != WSAEWOULDBLOCK)
				{
					Close();
					if (!m_bSentClose)
					{
						m_bSentClose = TRUE;
						m_pOwner->m_pOwner->PostThreadMessage(m_nInternalMessageID, FZAPI_THREADMSG_TRANSFEREND, m_nMode);
					}
				}
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
		if (!m_pFile)
		{
			return;
		}
		if (!m_pBuffer)
			m_pBuffer = new char[BUFSIZE];
		
		int numread;
		
		bool beenWaiting = false;
		_int64 currentBufferSize;
		if (GetState() != closed)
			currentBufferSize = m_pOwner->GetAbleToTransferSize(CControlSocket::upload, beenWaiting);
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
				if (ShutDown() || GetLastError() != WSAEWOULDBLOCK)
				{
					Close();
					if (!m_bSentClose)
					{
						m_bSentClose = TRUE;
						m_pOwner->m_pOwner->PostThreadMessage(m_nInternalMessageID, FZAPI_THREADMSG_TRANSFEREND, m_nMode);
					}
				}
				return;
			}
		}
		else
			numread = 0;
			
		ASSERT((numread+m_bufferpos) <= BUFSIZE);
		ASSERT(numread>=0);
		ASSERT(m_bufferpos>=0);

		if (numread+m_bufferpos <= 0)
		{
			if (ShutDown() || GetLastError()!=WSAEWOULDBLOCK)
			{
				Close();
				if (!m_bSentClose)
				{
					m_bSentClose=TRUE;
					m_pOwner->m_pOwner->PostThreadMessage(m_nInternalMessageID, FZAPI_THREADMSG_TRANSFEREND, m_nMode);
				}
			}
			return;
		}
			
		int numsent = Send(m_pBuffer, numread + m_bufferpos);
			
		while (TRUE)
		{
			if (numsent != SOCKET_ERROR)
			{
				Transfered(numsent, CTime::GetCurrentTime());
				m_pOwner->SpeedLimitAddTransferredBytes(CControlSocket::upload, numsent);
				m_LastActiveTime = CTime::GetCurrentTime();
				UpdateSendLed();
				m_transferdata.transferleft -= numsent;
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
					m_bufferpos += numread;
#ifndef MPEXT_NO_SSL
				else if (m_pSslLayer && nError == WSAESHUTDOWN)
				{
					m_bufferpos += numread;
					// Do nothing, wait for shutdown complete notification.
					return;
				}
#endif
				else
				{
					if (ShutDown() || GetLastError() != WSAEWOULDBLOCK)
					{
						Close();
						if (!m_bSentClose)
						{
							m_nMode |= CSMODE_TRANSFERERROR;
							m_bSentClose = TRUE;
							m_pOwner->m_pOwner->PostThreadMessage(m_nInternalMessageID, FZAPI_THREADMSG_TRANSFEREND, m_nMode);
						}
					}
				}
				UpdateStatusBar(false);
				return;
			}
			else
			{
				int pos = numread + m_bufferpos - numsent;

				if (pos < 0 || (numsent + pos) > BUFSIZE)
				{
					LogMessage(__FILE__, __LINE__, this, FZ_LOG_WARNING, _T("Index out of range"));
					if (ShutDown() || GetLastError() != WSAEWOULDBLOCK)
					{
						Close();
						if (!m_bSentClose)
						{
							m_nMode |= CSMODE_TRANSFERERROR;
							m_bSentClose = TRUE;
							m_pOwner->m_pOwner->PostThreadMessage(m_nInternalMessageID, FZAPI_THREADMSG_TRANSFEREND, m_nMode);
						}
					}
					return;
				}
				else if (!pos && numread < (currentBufferSize-m_bufferpos) && m_bufferpos != currentBufferSize)
				{
					if (ShutDown() || GetLastError() != WSAEWOULDBLOCK)
					{
						Close();
						if (!m_bSentClose)
						{
							m_bSentClose = TRUE;
							m_pOwner->m_pOwner->PostThreadMessage(m_nInternalMessageID, FZAPI_THREADMSG_TRANSFEREND, m_nMode);
						}
					}
					return;
				}
				else if (!pos)
					m_bufferpos = 0;
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
				LogMessage(__FILE__, __LINE__, this, FZ_LOG_DEBUG, _T("Message waiting in queue, resuming later"));
				TriggerEvent(FD_WRITE);
				UpdateStatusBar(false);
				return;
			}
			UpdateStatusBar(false);
				
			if (GetState() != closed)
				currentBufferSize = m_pOwner->GetAbleToTransferSize(CControlSocket::upload, beenWaiting);
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
					if (ShutDown() || GetLastError() != WSAEWOULDBLOCK)
					{
						Close();
						if (!m_bSentClose)
						{
							m_bSentClose = TRUE;
							m_pOwner->m_pOwner->PostThreadMessage(m_nInternalMessageID, FZAPI_THREADMSG_TRANSFEREND, m_nMode);
						}
					}
					return;
				}
			}
			else 
				numread = 0;

			if (!currentBufferSize && !m_bufferpos)
			{
				// Not allowed to send yet, try later
				TriggerEvent(FD_WRITE);
				return;
			}
	
			ASSERT(numread>=0);
			ASSERT(m_bufferpos>=0);
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
	CTimeSpan timespan=CTime::GetCurrentTime()-m_StartTime;
	int elapsed=(int)timespan.GetTotalSeconds();

	t_ffam_transferstatus *status=new t_ffam_transferstatus;
	status->bFileTransfer = m_nMode & (CSMODE_DOWNLOAD | CSMODE_UPLOAD);
#ifdef MPEXT
	status->transfersize = m_transferdata.transfersize;
#endif
	status->timeelapsed=elapsed;
	status->bytes=m_transferdata.transfersize-m_transferdata.transferleft;
	if (m_transferdata.transfersize>0 && !(m_nMode&CSMODE_LIST))
	{
		double leftmodifier=static_cast<double>(m_transferdata.transfersize-m_transferdata.nTransferStart-m_transferdata.transferleft);
		leftmodifier*=100;
		if (m_transferdata.transfersize-m_transferdata.nTransferStart)
			leftmodifier /= (m_transferdata.transfersize-m_transferdata.nTransferStart);
		else
			leftmodifier = 1;
		if (leftmodifier == 0)
			leftmodifier = 1;
		double leftmodifier2 = 100 - leftmodifier;
		int left=static_cast<int>((elapsed/leftmodifier)*leftmodifier2);
		double percent=100*static_cast<double>(m_transferdata.transfersize-m_transferdata.transferleft);
		percent/=m_transferdata.transfersize;
		status->percent=static_cast<int>(percent);
		if (status->percent>100)
			status->percent=100;
		
		if (left < 0)
			left = -1;
		status->timeleft=left;		
	}
	else
	{
		status->percent=-1;
		status->timeleft=-1;
	}

	int count = 0;
	status->transferrate = 0;

	for ( int i = 0; i < SPEED_SECONDS; i++)
	{
		if ( m_UsedForTransfer[ i])
		{
			status->transferrate += m_Transfered[ i];

			count++;
		}
	}

	if ( count > 0)
		status->transferrate = status->transferrate / count;
	else if (m_Transfered[0])
		status->transferrate = m_Transfered[0];
	else
		status->timeleft=-1;
		
	PostMessage(m_pOwner->m_pOwner->m_hOwnerWnd, m_pOwner->m_pOwner->m_nReplyMessageID, FZ_MSG_MAKEMSG(FZ_MSG_TRANSFERSTATUS, 0), (LPARAM)status);
}

void CTransferSocket::UpdateSendLed()
{
	//Don't flood the main window with messages
	//Else performance would be really low
	LARGE_INTEGER curtime;
	LARGE_INTEGER freq;
	QueryPerformanceFrequency(&freq);
	QueryPerformanceCounter(&curtime);
	static LARGE_INTEGER oldtime={0};
	if ( ( (curtime.QuadPart-oldtime.QuadPart) < (freq.QuadPart/15) ) )
		return;
	oldtime=curtime;
	
	PostMessage(m_pOwner->m_pOwner->m_hOwnerWnd, m_pOwner->m_pOwner->m_nReplyMessageID,	FZ_MSG_MAKEMSG(FZ_MSG_SOCKETSTATUS, FZ_SOCKETSTATUS_SEND), 0);
}

void CTransferSocket::UpdateRecvLed()
{
	//Don't flood the main window with messages
	//Else performance would be really low
	LARGE_INTEGER curtime;
	LARGE_INTEGER freq;
	QueryPerformanceFrequency(&freq);
	QueryPerformanceCounter(&curtime);
	static LARGE_INTEGER oldtime={0};
	if ( ( (curtime.QuadPart-oldtime.QuadPart) < (freq.QuadPart/15) ) )
		return;
	oldtime=curtime;
	
	PostMessage(m_pOwner->m_pOwner->m_hOwnerWnd, m_pOwner->m_pOwner->m_nReplyMessageID, FZ_MSG_MAKEMSG(FZ_MSG_SOCKETSTATUS, FZ_SOCKETSTATUS_RECV), 0);
}

BOOL CTransferSocket::Create(
#ifndef MPEXT_NO_SSL
  BOOL bUseSsl
#endif
	)
{
#ifndef MPEXT_NO_SSL
	if (bUseSsl)
		m_pSslLayer = new CAsyncSslSocketLayer;
#endif

	if (!m_pOwner->m_CurrentServer.fwbypass)
	{
		int nProxyType = COptions::GetOptionVal(OPTION_PROXYTYPE);
		if (nProxyType != PROXYTYPE_NOPROXY)
		{
			USES_CONVERSION;
			
			m_pProxyLayer = new CAsyncProxySocketLayer;
			if (nProxyType == PROXYTYPE_SOCKS4)
				m_pProxyLayer->SetProxy(PROXYTYPE_SOCKS4, T2CA(COptions::GetOption(OPTION_PROXYHOST)), COptions::GetOptionVal(OPTION_PROXYPORT));
			else if (nProxyType == PROXYTYPE_SOCKS4A)
				m_pProxyLayer->SetProxy(PROXYTYPE_SOCKS4A, T2CA(COptions::GetOption(OPTION_PROXYHOST)), COptions::GetOptionVal(OPTION_PROXYPORT));
			else if (nProxyType == PROXYTYPE_SOCKS5)
				if (COptions::GetOptionVal(OPTION_PROXYUSELOGON))
					m_pProxyLayer->SetProxy(PROXYTYPE_SOCKS5, T2CA(COptions::GetOption(OPTION_PROXYHOST)),
											COptions::GetOptionVal(OPTION_PROXYPORT),
											T2CA(COptions::GetOption(OPTION_PROXYUSER)),
											T2CA(CCrypt::decrypt(COptions::GetOption(OPTION_PROXYPASS))));
				else
					m_pProxyLayer->SetProxy(PROXYTYPE_SOCKS5, T2CA(COptions::GetOption(OPTION_PROXYHOST)),
											COptions::GetOptionVal(OPTION_PROXYPORT));
			else if (nProxyType == PROXYTYPE_HTTP11)
				if (COptions::GetOptionVal(OPTION_PROXYUSELOGON))
					m_pProxyLayer->SetProxy(PROXYTYPE_HTTP11, T2CA(COptions::GetOption(OPTION_PROXYHOST)), COptions::GetOptionVal(OPTION_PROXYPORT),
											T2CA(COptions::GetOption(OPTION_PROXYUSER)),
											T2CA(CCrypt::decrypt(COptions::GetOption(OPTION_PROXYPASS))));
				else
					m_pProxyLayer->SetProxy(PROXYTYPE_HTTP11, T2CA(COptions::GetOption(OPTION_PROXYHOST)), COptions::GetOptionVal(OPTION_PROXYPORT));
			else
				ASSERT(FALSE);
			AddLayer(m_pProxyLayer);
		}
	}

	if (!COptions::GetOptionVal(OPTION_LIMITPORTRANGE))
	{
		if (!CAsyncSocketEx::Create(0, SOCK_STREAM, FD_READ | FD_WRITE | FD_OOB | FD_ACCEPT | FD_CONNECT | FD_CLOSE, 0, GetFamily()))
			return FALSE;

		return TRUE;
	}
	else
	{
		int min=COptions::GetOptionVal(OPTION_PORTRANGELOW);
		int max=COptions::GetOptionVal(OPTION_PORTRANGEHIGH);
		if (min>=max)
		{
			m_pOwner->ShowStatus(IDS_ERRORMSG_CANTCREATEDUETOPORTRANGE,1);
			return FALSE;
		}
		int startport=static_cast<int>(min+((double)rand()*(max-min))/(RAND_MAX+1));
		int port=startport;
		while (!CAsyncSocketEx::Create(port, SOCK_STREAM, FD_READ | FD_WRITE | FD_OOB | FD_ACCEPT | FD_CONNECT | FD_CLOSE, 0, GetFamily()))
		{
			port++;
			if (port>max)
				port=min;
			if (port==startport)
			{
				m_pOwner->ShowStatus(IDS_ERRORMSG_CANTCREATEDUETOPORTRANGE,1);
				return FALSE;
			}
		}
	}
	
	return TRUE;
}

void CTransferSocket::Close()
{
	LogMessage(__FILE__, __LINE__, this,FZ_LOG_DEBUG, _T("Close()"));
	m_bCheckTimeout = FALSE;
	CAsyncSocketEx::Close();
}

int CTransferSocket::OnLayerCallback(std::list<t_callbackMsg>& callbacks)
{
	for (std::list<t_callbackMsg>::iterator iter = callbacks.begin(); iter != callbacks.end(); iter++)
	{
		if (iter->nType == LAYERCALLBACK_STATECHANGE)
		{
			if (iter->pLayer == m_pProxyLayer)
				LogMessage(__FILE__, __LINE__, this, FZ_LOG_INFO, _T("m_pProxyLayer changed state from %d to %d"), iter->nParam2, iter->nParam1);
#ifndef MPEXT_NO_SSL
			else if (iter->pLayer == m_pSslLayer)
				LogMessage(__FILE__, __LINE__, this, FZ_LOG_INFO, _T("m_pSslLayer changed state from %d to %d"), iter->nParam2, iter->nParam1);
#endif
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
					m_pOwner->ShowStatus(IDS_ERRORMSG_PROXY_NOCONN, 1);
					break;
				case PROXYERROR_REQUESTFAILED:
					m_pOwner->ShowStatus(IDS_ERRORMSG_PROXY_REQUESTFAILED, 1);
					break;
				case PROXYERROR_AUTHTYPEUNKNOWN:
					m_pOwner->ShowStatus(IDS_ERRORMSG_PROXY_AUTHTYPEUNKNOWN, 1);
					break;
				case PROXYERROR_AUTHFAILED:
					m_pOwner->ShowStatus(IDS_ERRORMSG_PROXY_AUTHFAILED, 1);
					break;
				case PROXYERROR_AUTHNOLOGON:
					m_pOwner->ShowStatus(IDS_ERRORMSG_PROXY_AUTHNOLOGON, 1);
					break;
				case PROXYERROR_CANTRESOLVEHOST:
					m_pOwner->ShowStatus(IDS_ERRORMSG_PROXY_CANTRESOLVEHOST, 1);
					break;
				default:
					LogMessage(__FILE__, __LINE__, this, FZ_LOG_WARNING, _T("Unknown proxy error"));
				}
			}
#ifndef MPEXT_NO_SSL
			else if (iter->pLayer == m_pSslLayer)
			{
				switch (iter->nParam1)
				{
				case SSL_INFO:
					switch(iter->nParam2)
					{
					case SSL_INFO_SHUTDOWNCOMPLETE:
						Close();
						if (!m_bSentClose)
						{
							m_bSentClose=TRUE;
							m_pOwner->m_pOwner->PostThreadMessage(m_nInternalMessageID, FZAPI_THREADMSG_TRANSFEREND, m_nMode);
						}
						break;
					case SSL_INFO_ESTABLISHED:
						m_pOwner->ShowStatus(IDS_STATUSMSG_SSLESTABLISHEDTRANSFER, 0);
						TriggerEvent(FD_FORCEREAD);
						break;
					}
					break;
				case SSL_FAILURE:
					switch (iter->nParam2)
					{
					case SSL_FAILURE_ESTABLISH:
						m_pOwner->ShowStatus(IDS_ERRORMSG_CANTESTABLISHSSLCONNECTION, 1);
						break;
#ifndef MPEXT_NO_SSLDLL
					case SSL_FAILURE_LOADDLLS:
						m_pOwner->ShowStatus(IDS_ERRORMSG_CANTLOADSSLDLLS, 1);
						break;
#endif
					case SSL_FAILURE_INITSSL:
						m_pOwner->ShowStatus(IDS_ERRORMSG_CANTINITSSL, 1);
						break;
					}
					if (!m_bSentClose)
					{
						m_nMode |= CSMODE_TRANSFERERROR;
						m_bSentClose = TRUE;
						m_pOwner->m_pOwner->PostThreadMessage(m_nInternalMessageID, FZAPI_THREADMSG_TRANSFEREND, m_nMode);
					}
					break;
				case SSL_VERIFY_CERT:
					t_SslCertData data;
					if (m_pSslLayer->GetPeerCertificateData(data))
						m_pSslLayer->SetNotifyReply(data.priv_data, SSL_VERIFY_CERT, 1);
					else
					{
						Close();
						if (!m_bSentClose)
						{
							m_nMode |= CSMODE_TRANSFERERROR;
							m_bSentClose = TRUE;
							m_pOwner->m_pOwner->PostThreadMessage(m_nInternalMessageID, FZAPI_THREADMSG_TRANSFEREND, m_nMode);
						}
					}
					break;
				}
			}
#endif
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
					Close();
					if (!m_bSentClose)
					{
						m_bSentClose = TRUE;
						m_pOwner->m_pOwner->PostThreadMessage(m_nInternalMessageID, FZAPI_THREADMSG_TRANSFEREND, m_nMode);
					}
					break;
				}
			}
#endif
		}
		delete [] iter->str;
	}
	return 0;
}

void CTransferSocket::Transfered(int count, CTime time)
{
	CTimeSpan ts = time - m_TransferedFirst;
	int diff = (int)ts.GetTotalSeconds();
	if (diff < 0)
		diff = 0;
	
	if ( diff >= SPEED_SECONDS)
	{
		int move = diff - SPEED_SECONDS + 1;
		int start = SPEED_SECONDS - move;

		if ( start <= 0)
			start = 0;
		else
		{
			for ( int i = 0; i < SPEED_SECONDS - move; i++)
			{
				m_Transfered[ i] = m_Transfered[ i + move];
				m_UsedForTransfer[ i] = m_UsedForTransfer[ i + move];
			}
		}

		for ( int i = start; i < SPEED_SECONDS; i++)
		{
			m_Transfered[ i] = 0;
			m_UsedForTransfer[ i] = false;
		}

		if (move >= SPEED_SECONDS)
		{
			m_TransferedFirst = time;
			diff = 0;
		}
		else
		{
			m_TransferedFirst += CTimeSpan( move);
			ts = time - m_TransferedFirst;
			diff = (int)(ts.GetTotalSeconds() % 60);
		}
	}

	m_Transfered[ diff] += count;

	for ( int i = 0; i < diff - 1; i++)
		m_UsedForTransfer[ i] = true;
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

int CTransferSocket::ReadDataFromFile(char *buffer, int len)
{
	TRY
	{
#ifndef MPEXT
		if (!m_transferdata.bType)
#endif
			return m_pFile->Read(buffer, len);
#ifndef MPEXT
		else
		{   //Read the file as ASCII file with CRLF line end
			if (!m_ReadBuffer)
				m_ReadBuffer = new char[BUFSIZE];
			if (!m_ReadSize)
				m_ReadSize = m_pFile->Read(m_ReadBuffer, len);
			int numread = 0;
			while (numread < len)
			{
				if (m_ReadPos >= m_ReadSize)
				{
					m_ReadSize = m_pFile->Read(m_ReadBuffer, len);
					m_ReadPos = 0;
					if (!m_ReadSize)
					    break;
				}
				if (m_ReadBuffer[m_ReadPos] == '\n' && m_cLastChar != '\r')
				{
					buffer[numread++] = '\r';
					m_cLastChar = '\r';
					if (numread == len)
					    break;
				}

				buffer[numread] = m_ReadBuffer[m_ReadPos];
				m_cLastChar = buffer[numread];
				numread++;
				m_ReadPos++;
			}
			ASSERT(numread <= len);
			return numread;
		}
#endif
	}
	CATCH_ALL(e)
	{
		TCHAR error[BUFSIZE];
		if (e->GetErrorMessage(error, BUFSIZE))
			m_pOwner->ShowStatus(error, 1);
		if (ShutDown() || GetLastError() != WSAEWOULDBLOCK)
		{
			Close();
			if (!m_bSentClose)
			{
				m_nMode |= CSMODE_TRANSFERERROR;
				m_bSentClose = TRUE;
				m_pOwner->m_pOwner->PostThreadMessage(m_nInternalMessageID, FZAPI_THREADMSG_TRANSFEREND, m_nMode);
			}
		}
		return -1;
	}
	END_CATCH_ALL;
}