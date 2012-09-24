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

// MainThread.cpp: Implementierungsdatei
//

#include "stdafx.h"
#include "MainThread.h"
#ifndef MPEXT
#include "fileexistsdlg.h"
#include "resource.h"
#include "entersomething.h"
#endif
#ifndef MPEXT_NO_CACHE
#include "directorycache.h"
#endif

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define ECS m_CriticalSection.Lock()
#define LCS m_CriticalSection.Unlock()

/////////////////////////////////////////////////////////////////////////////
// CMainThread

CMainThread::CMainThread()
{
	m_hOwnerWnd = 0;
	m_nReplyMessageID = 0;
	m_nInternalMessageID = 0;
	m_pPostKeepAliveCommand = 0;
	m_nTimerID = 0;
	m_pControlSocket = NULL;
	m_pFtpControlSocket = NULL;
#ifndef MPEXT_NO_SFTP
	m_pSFtpControlSocket = NULL;
#endif
	m_bBusy = FALSE;
	m_bConnected = FALSE;
#ifndef MPEXT_NO_CACHE
	m_pDirectoryCache = 0;
#endif
	m_pWorkingDir = 0;
	m_nAsyncRequestID = 0;
	m_bQuit = FALSE;
#ifndef MPEXT_NO_IDENT
	m_pIdentServer = 0;
#endif
	m_hThread = 0;
	m_dwThreadId = 0;
}

CMainThread::~CMainThread()
{
	delete m_pWorkingDir;
	CloseHandle(m_hThread);
}

BOOL CMainThread::InitInstance()
{	
	m_nTimerID=SetTimer(0,1,1000,0);
#ifndef MPEXT_NO_CACHE
	m_pDirectoryCache=new CDirectoryCache;					
#endif
	m_pPostKeepAliveCommand=0;
	
	// initialize Winsock library
	BOOL res=TRUE;
	WSADATA wsaData;
	
	WORD wVersionRequested = MAKEWORD(1, 1);
	int nResult = WSAStartup(wVersionRequested, &wsaData);
	if (nResult != 0)
		res=FALSE;
	else if (LOBYTE(wsaData.wVersion) != 1 || HIBYTE(wsaData.wVersion) != 1)
	{
		WSACleanup();
		res=FALSE;
	}
	
	m_pFtpControlSocket=new CFtpControlSocket(this);
#ifndef MPEXT_NO_SFTP
	m_pSFtpControlSocket=new CSFtpControlSocket(this);
#endif
	m_pControlSocket=m_pFtpControlSocket;
	m_pFtpControlSocket->InitLog(this);
#ifndef MPEXT_NO_SFTP
	m_pSFtpControlSocket->InitLog(this);
#endif
#ifndef MPEXT_NO_IDENT
	if (COptions::GetOptionVal(OPTION_IDENT) && !COptions::GetOptionVal(OPTION_IDENTCONNECT))
		m_pIdentServer=new CIdentServerControl(this);
#endif
	return TRUE;
}

DWORD CMainThread::ExitInstance()
{
	KillTimer(0,m_nTimerID);
	if (m_pFtpControlSocket)
		delete m_pFtpControlSocket;
#ifndef MPEXT_NO_SFTP
	if (m_pSFtpControlSocket)
		delete m_pSFtpControlSocket;
#endif
#ifndef MPEXT_NO_CACHE
	if (m_pDirectoryCache)
		delete m_pDirectoryCache;
#endif
#ifndef MPEXT_NO_IDENT
	if (m_pIdentServer)
		delete m_pIdentServer;
#endif
	return 1;
}

BOOL CMainThread::IsConnected()
{
	BOOL bConnected;
	ECS;
	bConnected=m_bConnected;
	LCS;
	return bConnected;
}

void CMainThread::OnTimer(WPARAM wParam, LPARAM lParam)
{
	if (!m_pControlSocket)
		return;
	
	if (wParam==m_nTimerID)
		m_pControlSocket->OnTimer();
	
	return;
}

void CMainThread::ShowStatus(CString status, int type)
{
	ECS;
	if (m_bQuit)
	{
		LCS;
		return;
	}
	LCS;
	//Displays a message in the message log
	t_ffam_statusmessage *pStatus = new t_ffam_statusmessage;
	pStatus->post = TRUE;
	pStatus->status = status;
	pStatus->type = type;
	if (!PostMessage(m_hOwnerWnd, m_nReplyMessageID, FZ_MSG_MAKEMSG(FZ_MSG_STATUS, 0), (LPARAM)pStatus))
		delete pStatus;
}

void CMainThread::ShowStatus(UINT nID, int type)
{
	ECS;
	if (m_bQuit)
	{
		LCS;
		return;
	}
	LCS;
	CString str;
	str.LoadString(nID);
	ShowStatus(str,type);
}

BOOL CMainThread::OnThreadMessage(UINT Msg, WPARAM wParam, LPARAM lParam)
{
	if (Msg==m_nInternalMessageID)
	{
		if (wParam==FZAPI_THREADMSG_COMMAND)
		{
			if (m_pControlSocket && !m_pControlSocket->IsReady())
				m_pPostKeepAliveCommand=(t_command *)lParam;
			else
			{
				t_command *pCommand=(t_command *)lParam;
				switch(pCommand->id)
				{
				case FZ_COMMAND_CONNECT:
					ASSERT(!IsConnected());
					SetCurrentPath(CServerPath());
#ifndef MPEXT_NO_SFTP
					if (pCommand->server.nServerType&FZ_SERVERTYPE_SUB_FTP_SFTP)
						m_pControlSocket=m_pSFtpControlSocket;
					else
#endif
						m_pControlSocket=m_pFtpControlSocket;
					ASSERT(m_pControlSocket);
					m_pControlSocket->Connect(pCommand->server);
					break;
				case FZ_COMMAND_LIST:
					ASSERT(m_pControlSocket);
					m_pControlSocket->List(FALSE, 0, pCommand->path, pCommand->param1, pCommand->param4);
					break;
				case FZ_COMMAND_LISTFILE:
					ASSERT(m_pControlSocket);
					m_pControlSocket->ListFile(pCommand->path, pCommand->param1);
					break;
				case FZ_COMMAND_FILETRANSFER:
					ASSERT(m_pControlSocket);
					m_pControlSocket->FileTransfer(&pCommand->transferfile);
					break;
				case FZ_COMMAND_CUSTOMCOMMAND:
					ASSERT(m_pControlSocket);
					m_pControlSocket->FtpCommand(pCommand->param1);
					break;
				case FZ_COMMAND_DELETE:
					ASSERT(m_pControlSocket);
					m_pControlSocket->Delete(pCommand->param1, pCommand->path);
					break;
				case FZ_COMMAND_REMOVEDIR:
					ASSERT(m_pControlSocket);
					m_pControlSocket->RemoveDir(pCommand->param1, pCommand->path);
					break;
				case FZ_COMMAND_MAKEDIR:
					ASSERT(m_pControlSocket);
					m_pControlSocket->MakeDir(pCommand->path);
					break;
				case FZ_COMMAND_RENAME:
					ASSERT(m_pControlSocket);
					m_pControlSocket->Rename(pCommand->param1, pCommand->param2, pCommand->path, pCommand->newPath);
					break;
				case FZ_COMMAND_CHMOD:
					ASSERT(m_pControlSocket);
					m_pControlSocket->Chmod(pCommand->param1, pCommand->path, pCommand->param4);
					break;
				}
				delete pCommand;
			}
		}
		else if (wParam==FZAPI_THREADMSG_PROCESSREPLY)
			m_pControlSocket->ProcessReply();
		else if (wParam==FZAPI_THREADMSG_TRANSFEREND)
			m_pControlSocket->TransferEnd(lParam);
		else if (wParam==FZAPI_THREADMSG_CANCEL)
			m_pControlSocket->Cancel(lParam);
		else if (wParam==FZAPI_THREADMSG_DISCONNECT)
			m_pControlSocket->Disconnect();
		else if (wParam==FZAPI_THREADMSG_POSTKEEPALIVE)
		{
			if (m_pPostKeepAliveCommand)
			{
				PostThreadMessage(m_nInternalMessageID,FZAPI_THREADMSG_COMMAND,(LPARAM)m_pPostKeepAliveCommand);
				m_pPostKeepAliveCommand=0;
			}
		}
		else if (wParam==FZAPI_THREADMSG_ASYNCREQUESTREPLY)
		{
			CAsyncRequestData *pData=(CAsyncRequestData *)lParam;
			if (pData)
			{
				if (pData->nRequestID!=GetAsyncRequestID())
					LogMessage(__FILE__, __LINE__, this,FZ_LOG_INFO, _T("Ignoring old request ID"));
				else
					m_pControlSocket->SetAsyncRequestResult(pData->nRequestResult, pData);
				delete pData;
			}
			else
				LogMessage(__FILE__, __LINE__, this,FZ_LOG_WARNING, _T("Request reply without data"));
		}
		return TRUE;
	}
	else if (Msg==WM_TIMER)
	{
		OnTimer(wParam, lParam);
	}
	
	return TRUE;
}

BOOL CMainThread::IsBusy()
{
	BOOL bBusy;
	ECS;
	bBusy=m_bBusy;
	LCS;
	return bBusy;
}

void CMainThread::Command(const t_command &command)
{
	ASSERT(!IsBusy());
	ECS;
	if (m_bQuit)
	{
		LCS;
		return;
	}
	m_bBusy=TRUE;
	t_command *pCommand=new t_command;
	*pCommand=command;
	VERIFY(PostThreadMessage(m_nInternalMessageID,FZAPI_THREADMSG_COMMAND,(LPARAM)pCommand));
	m_LastCommand=command;
	LCS;
}

BOOL CMainThread::LastOperationSuccessful()
{
	return TRUE;
}

void CMainThread::SetBusy(BOOL bBusy)
{
	ECS;
	m_bBusy=bBusy;
	LCS;
}

void CMainThread::SetConnected(BOOL bConnected /*=TRUE*/)
{
	ECS;
	m_bConnected=bConnected;
#ifdef MPEXT
	if (!bConnected)
	{
		// when we loose connection
		// reset pending commands as we cannot fulfill them anyway
		m_pPostKeepAliveCommand = 0;
	}
#endif
	LCS;
}

bool CMainThread::GetCurrentPath(CServerPath &dir)
{
	if (!IsConnected())
		return false;
	ECS;
	dir=m_CurrentPath;
	LCS;
	return true;
}

CServerPath CMainThread::GetCurrentPath()
{
	CServerPath path;
	bool res = GetCurrentPath(path);
	if (!res)
		return CServerPath();
	
	return path;
}

BOOL CMainThread::GetCurrentServer(t_server &server)
{
	if (!IsConnected())
		return FALSE;
	ECS;
	server=m_pControlSocket->GetCurrentServer();
	LCS;
	return TRUE;
}

void CMainThread::Quit()
{
	ECS;
	m_bQuit=TRUE;
	LCS;
	if (IsBusy())
		PostThreadMessage(m_nInternalMessageID, FZAPI_THREADMSG_CANCEL, 1);
	PostThreadMessage(WM_QUIT, 0, 0);
}

void CMainThread::SetCurrentPath(CServerPath path)
{
	ECS;
	m_CurrentPath=path;
	LCS;
	return;
}

#ifndef MPEXT
int CMainThread::GetOption(int nOption)
{
	int nValue=0;
	ECS;
	std::map<int, int>::iterator iter=m_Options.find(nOption);
	if (iter!=m_Options.end())
		nValue=iter->second;
	LCS;
	return nValue;
}

void CMainThread::SetOption(int nOption, int nValue)
{
	ECS;
	m_Options[nOption]=nValue;
	LCS;
}
#endif

BOOL CMainThread::GetWorkingDir(t_directory *pWorkingDir)
{
	ECS;
	if (m_pWorkingDir)
	{
		*pWorkingDir=*m_pWorkingDir;
		LCS;
		return TRUE;
	}
	LCS;
	return FALSE;
}

void CMainThread::SetWorkingDir(t_directory *pWorkingDir)
{
	if (!pWorkingDir)
	{
		ECS;
		delete m_pWorkingDir;
		m_pWorkingDir=0;
		LCS;
	}
	else
	{
		ECS;
		if (!m_pWorkingDir)
			m_pWorkingDir=new t_directory;
		*m_pWorkingDir=*pWorkingDir;
		LCS;
	}
	if (pWorkingDir)
	{
		t_directory *pDirectoryToSend=new t_directory;
		*pDirectoryToSend=*pWorkingDir;
		PostMessage(m_hOwnerWnd, m_nReplyMessageID, FZ_MSG_MAKEMSG(FZ_MSG_LISTDATA, 0), (LPARAM)pDirectoryToSend);
	}

	return;
}

bool CMainThread::GetWorkingDirPath(CServerPath &path)
{
	ECS;
	if (m_pWorkingDir)
	{
		path = m_pWorkingDir->path;
		LCS;
		return true;
	}
	LCS;

	return false;
}

__int64 CMainThread::GetAsyncRequestID() const
{
	return m_nAsyncRequestID;
}

__int64 CMainThread::GetNextAsyncRequestID()
{
	return ++m_nAsyncRequestID;
}

CMainThread* CMainThread::Create(int nPriority /*=THREAD_PRIORITY_NORMAL*/, DWORD dwCreateFlags /*=0*/)
{
	CMainThread *pMainThread=new CMainThread();
	pMainThread->m_hThread=CreateThread(0, 0, ThreadProc, pMainThread, dwCreateFlags, &pMainThread->m_dwThreadId);
	if (!pMainThread->m_hThread)
	{
		delete pMainThread;
		return NULL;
	}
	::SetThreadPriority(pMainThread->m_hThread, nPriority);
	return pMainThread;
}

BOOL CMainThread::PostThreadMessage(UINT message, WPARAM wParam, LPARAM lParam)
{
	return ::PostThreadMessage(m_dwThreadId, message, wParam, lParam);
}

DWORD CMainThread::ResumeThread()
{
	BOOL res=::ResumeThread(m_hThread);
	if (res)
	{
		m_EventStarted.Lock();
		m_EventStarted.Unlock();
	}
	return res;
}

DWORD WINAPI CMainThread::ThreadProc(LPVOID lpParameter)
{
	return ((CMainThread *)lpParameter)->Run();
}

DWORD CMainThread::Run()
{
	ECS;
	InitInstance();
	m_EventStarted.SetEvent();
	LCS;
	MSG msg;
	while (GetMessage(&msg, 0, 0, 0))
	{
		TranslateMessage(&msg);
		if (!msg.hwnd)
			OnThreadMessage(msg.message, msg.wParam, msg.lParam);
		DispatchMessage(&msg);
	}
	DWORD res = ExitInstance();
	delete this;
	return res;
}

BOOL CMainThread::IsValid() const
{
	if (!this)
		return FALSE;

	if (IsBadWritePtr((VOID *)this, sizeof(CMainThread)) )
		return FALSE;

	if (m_pControlSocket &&
		m_pControlSocket != m_pFtpControlSocket
#ifndef MPEXT_NO_SFTP
		&&
		m_pControlSocket != m_pSFtpControlSocket
#endif
		)
		return FALSE;

	return TRUE;
}