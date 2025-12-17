//---------------------------------------------------------------------------
#include "FileZillaPCH.h"
#include "MainThread.h"

#define ECS m_CriticalSection.Lock()
#define LCS m_CriticalSection.Unlock()

/////////////////////////////////////////////////////////////////////////////
// CMainThread

CMainThread::CMainThread()
{
  m_pTools = NULL;
  m_nInternalMessageID = 0;
  m_pPostKeepAliveCommand = 0;
  m_nTimerID = 0;
  m_pControlSocket = NULL;
  m_bBusy = FALSE;
  m_bConnected = FALSE;
  m_pWorkingDir = 0;
  m_nAsyncRequestID = 0;
  m_bQuit = FALSE;
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

  m_pControlSocket=new CFtpControlSocket(this, m_pTools);
  m_pControlSocket->InitIntern(GetIntern());
  return TRUE;
}

DWORD CMainThread::ExitInstance()
{
  KillTimer(0,m_nTimerID);
  if (m_pControlSocket)
    delete m_pControlSocket;
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

void CMainThread::OnTimer(WPARAM wParam, LPARAM)
{
  if (!m_pControlSocket)
    return;

  if (wParam==m_nTimerID)
    m_pControlSocket->OnTimer();

  return;
}

BOOL CMainThread::OnThreadMessage(UINT Msg, WPARAM wParam, LPARAM lParam)
{
  if (Msg==m_nInternalMessageID)
  {
    if (wParam==FZAPI_THREADMSG_COMMAND)
    {
      t_command *pCommand=reinterpret_cast<t_command *>(lParam);
      if (m_pControlSocket && !m_pControlSocket->IsReady())
        m_pPostKeepAliveCommand=pCommand;
      else
      {
        switch(pCommand->id)
        {
        case FZ_COMMAND_CONNECT:
          DebugAssert(!IsConnected());
          SetCurrentPath(CServerPath());
          m_pControlSocket->Connect(pCommand->server);
          break;
        case FZ_COMMAND_LIST:
          DebugAssert(m_pControlSocket);
          m_pControlSocket->List(FALSE, 0, pCommand->path, pCommand->param1);
          break;
        case FZ_COMMAND_LISTFILE:
          DebugAssert(m_pControlSocket);
          m_pControlSocket->ListFile(pCommand->param1, pCommand->path);
          break;
        case FZ_COMMAND_FILETRANSFER:
          DebugAssert(m_pControlSocket);
          m_pControlSocket->FileTransfer(&pCommand->transferfile);
          break;
        case FZ_COMMAND_CUSTOMCOMMAND:
          DebugAssert(m_pControlSocket);
          m_pControlSocket->FtpCommand(pCommand->param1);
          break;
        case FZ_COMMAND_DELETE:
          DebugAssert(m_pControlSocket);
          m_pControlSocket->Delete(pCommand->param1, pCommand->path, (pCommand->param4 != 0));
          break;
        case FZ_COMMAND_REMOVEDIR:
          DebugAssert(m_pControlSocket);
          m_pControlSocket->RemoveDir(pCommand->param1, pCommand->path);
          break;
        case FZ_COMMAND_MAKEDIR:
          DebugAssert(m_pControlSocket);
          m_pControlSocket->MakeDir(pCommand->path);
          break;
        case FZ_COMMAND_RENAME:
          DebugAssert(m_pControlSocket);
          m_pControlSocket->Rename(pCommand->param1, pCommand->param2, pCommand->path, pCommand->newPath);
          break;
        case FZ_COMMAND_CHMOD:
          DebugAssert(m_pControlSocket);
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
        PostThreadMessage(m_nInternalMessageID,FZAPI_THREADMSG_COMMAND,reinterpret_cast<LPARAM>(m_pPostKeepAliveCommand));
        m_pPostKeepAliveCommand=0;
      }
    }
    else if (wParam==FZAPI_THREADMSG_ASYNCREQUESTREPLY)
    {
      CAsyncRequestData *pData=reinterpret_cast<CAsyncRequestData *>(lParam);
      if (pData)
      {
        if (pData->nRequestID!=GetAsyncRequestID())
          LogMessage(FZ_LOG_INFO, L"Ignoring old request ID");
        else
          m_pControlSocket->SetAsyncRequestResult(pData->nRequestResult, pData);
        delete pData;
      }
      else
        LogMessage(FZ_LOG_WARNING, L"Request reply without data");
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
  DebugAssert(!IsBusy());
  ECS;
  if (m_bQuit)
  {
    LCS;
    return;
  }
  m_bBusy=TRUE;
  t_command *pCommand=new t_command;
  *pCommand=command;
  DebugCheck(PostThreadMessage(m_nInternalMessageID,FZAPI_THREADMSG_COMMAND,reinterpret_cast<LPARAM>(pCommand)));
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
  if (!bConnected)
  {
    // when we loose connection
    // reset pending commands as we cannot fulfill them anyway
    m_pPostKeepAliveCommand = 0;
  }
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

bool CMainThread::UsingMlsd()
{
  if (!IsConnected())
    return false;
  return m_pControlSocket->UsingMlsd();
}

bool CMainThread::UsingUtf8()
{
  if (!IsConnected())
    return false;
  return m_pControlSocket->UsingUtf8();
}

std::string CMainThread::GetTlsVersionStr()
{
  if (!IsConnected())
    return std::string();
  return m_pControlSocket->GetTlsVersionStr();
}

std::string CMainThread::GetCipherName()
{
  if (!IsConnected())
    return std::string();
  return m_pControlSocket->GetCipherName();
}

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
    SendDirectoryListing(pDirectoryToSend);
  }

  return;
}

void CMainThread::SendDirectoryListing(t_directory * pDirectoryToSend)
{
  if (!GetIntern()->PostMessage(FZ_MSG_MAKEMSG(FZ_MSG_LISTDATA, 0), reinterpret_cast<LPARAM>(pDirectoryToSend)))
  {
    delete pDirectoryToSend;
  }
}

__int64 CMainThread::GetAsyncRequestID() const
{
  return m_nAsyncRequestID;
}

__int64 CMainThread::GetNextAsyncRequestID()
{
  return ++m_nAsyncRequestID;
}

CMainThread* CMainThread::Create(int nPriority, DWORD dwCreateFlags)
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
  m_Started = false;
  BOOL res=::ResumeThread(m_hThread);
  if (res)
  {
    while (!m_Started)
    {
      Sleep(10);
    }
  }
  return res;
}

DWORD WINAPI CMainThread::ThreadProc(LPVOID lpParameter)
{
  return static_cast<CMainThread *>(lpParameter)->Run();
}

DWORD CMainThread::Run()
{
  ECS;
  InitInstance();
  m_Started = true;
  LCS;
  MSG msg;
  while (GetMessage(&msg, 0, 0, 0))
  {
    TranslateMessage(&msg);
    if (!msg.hwnd)
    {
      OnThreadMessage(msg.message, msg.wParam, msg.lParam);
    }
    DispatchMessage(&msg);
  }
  DWORD res = ExitInstance();
  delete this;
  return res;
}
