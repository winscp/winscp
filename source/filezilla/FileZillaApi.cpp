//---------------------------------------------------------------------------
#include "stdafx.h"
#include "FileZillaApi.h"
#include "MainThread.h"

//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

CFileZillaApi::CFileZillaApi()
{
  m_nInternalMessageID=0;
  m_pMainThread=0;
  m_bInitialized=FALSE;
}

CFileZillaApi::~CFileZillaApi()
{
  Destroy();
}

int CFileZillaApi::Init(TFileZillaIntern * Intern, CFileZillaTools * pTools)
{
  //Check if call allowed
  if (m_bInitialized)
    return FZ_REPLY_ALREADYINIZIALIZED;

  //Initialize variables
  m_nInternalMessageID=RegisterWindowMessage( L"FileZillaInternalApiMessage{F958620E-040C-4b33-A091-7E04E10AA660}" );
  if (!m_nInternalMessageID)
    return FZ_REPLY_NOTINITIALIZED;

  //Create thread object
  m_pMainThread = CMainThread::Create(THREAD_PRIORITY_BELOW_NORMAL, CREATE_SUSPENDED);

  //Initialize Thread variables
  m_pMainThread->m_nInternalMessageID=m_nInternalMessageID;
  m_pMainThread->m_pTools=pTools;

  m_pMainThread->InitIntern(Intern);

  //Resume Thread
  m_pMainThread->ResumeThread();

  //Initialization OK
  m_bInitialized=TRUE;
  return FZ_REPLY_OK;
}

int CFileZillaApi::IsConnected()
{
  if (!m_bInitialized)
    return FZ_REPLY_NOTINITIALIZED;
  return m_pMainThread->IsConnected()?FZ_REPLY_OK:FZ_REPLY_NOTCONNECTED;
}

int CFileZillaApi::IsBusy()
{
  if (!m_bInitialized)
    return FZ_REPLY_NOTINITIALIZED;
  return m_pMainThread->IsBusy()?FZ_REPLY_BUSY:FZ_REPLY_IDLE;
}

int CFileZillaApi::Connect(const t_server &server)
{
  //Check parameters
  if (server.host==L"" || server.port<1 || server.port>65535)
    return FZ_REPLY_INVALIDPARAM;

#ifndef MPEXT_NO_GSS
  BOOL bUseGSS = FALSE;
  if (GetOptionVal(OPTION_USEGSS))
  {
    USES_CONVERSION;

    CString GssServers = GetOption(OPTION_GSSSERVERS);
    hostent *fullname = gethostbyname(T2CA(server.host));
    CString host;
    if (fullname)
      host = fullname->h_name;
    else
      host = server.host;
    host.MakeLower();
    int i;
    while ((i=GssServers.Find( L";" ))!=-1)
    {
      if ((L"."+GssServers.Left(i))==host.Right(GssServers.Left(i).GetLength()+1) || GssServers.Left(i)==host)
      {
        bUseGSS = TRUE;
        break;
      }
      GssServers = GssServers.Mid(i+1);
    }
  }
  if (!bUseGSS && server.user == L"")
    return FZ_REPLY_INVALIDPARAM;
#endif

  if (!(server.nServerType&FZ_SERVERTYPE_HIGHMASK))
    return FZ_REPLY_INVALIDPARAM;

  //Check if call allowed
  if (!m_bInitialized)
    return FZ_REPLY_NOTINITIALIZED;
  if (m_pMainThread->IsBusy())
    return FZ_REPLY_BUSY;

  t_command command;
  command.id=FZ_COMMAND_CONNECT;
  command.server=server;
  m_pMainThread->Command(command);
  return m_pMainThread->LastOperationSuccessful()?FZ_REPLY_OK:FZ_REPLY_ERROR;
}

int CFileZillaApi::Cancel()
{
  //Check if call allowed
  if (!m_bInitialized)
    return FZ_REPLY_NOTINITIALIZED;
  if (IsBusy()!=FZ_REPLY_BUSY)
    return FZ_REPLY_NOTBUSY;
  m_pMainThread->PostThreadMessage(m_nInternalMessageID, FZAPI_THREADMSG_CANCEL, 0);
  return FZ_REPLY_WOULDBLOCK;
}

void CFileZillaApi::Destroy()
{
  if (!m_bInitialized)
    return;
  DebugAssert(m_pMainThread);
  HANDLE tmp=m_pMainThread->m_hThread;
  m_pMainThread->Quit();
  //Wait for the main thread to quit
  WaitForSingleObject(tmp, 10000);

  m_pMainThread=0;
  m_bInitialized=FALSE;
}

int CFileZillaApi::Disconnect()
{
  //Check if call allowed
  if (!m_bInitialized)
    return FZ_REPLY_NOTINITIALIZED;
  if (IsConnected()==FZ_REPLY_NOTCONNECTED)
    return FZ_REPLY_NOTCONNECTED;
  if (IsBusy()==FZ_REPLY_BUSY)
    return FZ_REPLY_BUSY;

  m_pMainThread->PostThreadMessage(m_nInternalMessageID,FZAPI_THREADMSG_DISCONNECT,0);
  return FZ_REPLY_WOULDBLOCK;
}

int CFileZillaApi::List(const CServerPath& path)
{
  //Check if call allowed
  if (!m_bInitialized)
    return FZ_REPLY_NOTINITIALIZED;
  if (IsConnected()==FZ_REPLY_NOTCONNECTED)
    return FZ_REPLY_NOTCONNECTED;
  if (path.IsEmpty() && !m_pMainThread->GetIntern()->GetOptionVal(OPTION_MPEXT_WORK_FROM_CWD))
    return FZ_REPLY_INVALIDPARAM;

  if (m_pMainThread->IsBusy())
    return FZ_REPLY_BUSY;

  t_command command;
  command.id=FZ_COMMAND_LIST;
  command.path=path;
  m_pMainThread->Command(command);
  return m_pMainThread->LastOperationSuccessful()?FZ_REPLY_OK:FZ_REPLY_ERROR;
}

int CFileZillaApi::ListFile(CString FileName, const CServerPath & path)
{
  //Check if call allowed
  if (!m_bInitialized)
    return FZ_REPLY_NOTINITIALIZED;
  if (IsConnected()==FZ_REPLY_NOTCONNECTED)
    return FZ_REPLY_NOTCONNECTED;
  if (path.IsEmpty())
    return FZ_REPLY_INVALIDPARAM;
  if (FileName=="")
    return FZ_REPLY_INVALIDPARAM;

  if (m_pMainThread->IsBusy())
    return FZ_REPLY_BUSY;
  t_command command;
  command.id=FZ_COMMAND_LISTFILE;
  command.param1=FileName;
  command.path=path;
  m_pMainThread->Command(command);
  return m_pMainThread->LastOperationSuccessful()?FZ_REPLY_OK:FZ_REPLY_ERROR;
}

int CFileZillaApi::FileTransfer(const t_transferfile &TransferFile)
{
  //Check if call allowed
  if (!m_bInitialized)
    return FZ_REPLY_NOTINITIALIZED;
  if (IsConnected()==FZ_REPLY_NOTCONNECTED)
    return FZ_REPLY_NOTCONNECTED;
  if (TransferFile.remotefile==L"" || TransferFile.localfile==L"" || (TransferFile.remotepath.IsEmpty() && !m_pMainThread->GetIntern()->GetOptionVal(OPTION_MPEXT_WORK_FROM_CWD)))
    return FZ_REPLY_INVALIDPARAM;
  if (IsBusy()==FZ_REPLY_BUSY)
    return FZ_REPLY_BUSY;
  t_command command;
  command.id=FZ_COMMAND_FILETRANSFER;
  command.transferfile=TransferFile;
  m_pMainThread->Command(command);
  return m_pMainThread->LastOperationSuccessful()?FZ_REPLY_OK:FZ_REPLY_ERROR;
}

int CFileZillaApi::GetCurrentServer(t_server &server)
{
  //Check if call allowed
  if (!m_bInitialized)
    return FZ_REPLY_NOTINITIALIZED;
  if (IsConnected()==FZ_REPLY_NOTCONNECTED)
    return FZ_REPLY_NOTCONNECTED;
  if (m_pMainThread->GetCurrentServer(server))
    return FZ_REPLY_OK;
  else
    return FZ_REPLY_NOTCONNECTED;
}

int CFileZillaApi::SetCurrentPath(CServerPath path)
{
  //Check if call allowed
  if (!m_bInitialized)
    return FZ_REPLY_NOTINITIALIZED;
  if (IsConnected()==FZ_REPLY_NOTCONNECTED)
    return FZ_REPLY_NOTCONNECTED;
  m_pMainThread->SetCurrentPath(path);
  return FZ_REPLY_OK;
}

int CFileZillaApi::GetCurrentPath(CServerPath & path)
{
  //Check if call allowed
  if (!m_bInitialized)
    return FZ_REPLY_NOTINITIALIZED;
  if (IsConnected()==FZ_REPLY_NOTCONNECTED)
    return FZ_REPLY_NOTCONNECTED;
  return (m_pMainThread->GetCurrentPath(path) ? FZ_REPLY_OK : FZ_REPLY_NOTCONNECTED);
}

bool CFileZillaApi::UsingMlsd()
{
  //Check if call allowed
  if (!m_bInitialized)
    return false;
  if (IsConnected()==FZ_REPLY_NOTCONNECTED)
    return false;
  return m_pMainThread->UsingMlsd();
}

bool CFileZillaApi::UsingUtf8()
{
  //Check if call allowed
  if (!m_bInitialized)
    return false;
  if (IsConnected()==FZ_REPLY_NOTCONNECTED)
    return false;
  return m_pMainThread->UsingUtf8();
}

std::string CFileZillaApi::GetTlsVersionStr()
{
  //Check if call allowed
  if (!m_bInitialized)
    return std::string();
  if (IsConnected()==FZ_REPLY_NOTCONNECTED)
    return std::string();
  return m_pMainThread->GetTlsVersionStr();
}

std::string CFileZillaApi::GetCipherName()
{
  //Check if call allowed
  if (!m_bInitialized)
    return std::string();
  if (IsConnected()==FZ_REPLY_NOTCONNECTED)
    return std::string();
  return m_pMainThread->GetCipherName();
}

int CFileZillaApi::CustomCommand(CString CustomCommand)
{
  //Check if call allowed
  if (!m_bInitialized)
    return FZ_REPLY_NOTINITIALIZED;
  if (IsConnected()==FZ_REPLY_NOTCONNECTED)
    return FZ_REPLY_NOTCONNECTED;
  if (IsBusy()==FZ_REPLY_BUSY)
    return FZ_REPLY_BUSY;
  t_server server;
  int res=GetCurrentServer(server);
  if (res!=FZ_REPLY_OK)
    return res;
  if (CustomCommand==L"")
    return FZ_REPLY_INVALIDPARAM;

  t_command command;
  command.id=FZ_COMMAND_CUSTOMCOMMAND;
  command.param1=CustomCommand;
  m_pMainThread->Command(command);
  return m_pMainThread->LastOperationSuccessful()?FZ_REPLY_OK:FZ_REPLY_ERROR;
}

int CFileZillaApi::Delete(CString FileName, const CServerPath &path, bool filenameOnly)
{
  //Check if call allowed
  if (!m_bInitialized)
    return FZ_REPLY_NOTINITIALIZED;
  if (IsConnected()==FZ_REPLY_NOTCONNECTED)
    return FZ_REPLY_NOTCONNECTED;
  if (IsBusy()==FZ_REPLY_BUSY)
    return FZ_REPLY_BUSY;
  if (FileName=="")
    return FZ_REPLY_INVALIDPARAM;

  CServerPath path2=path;
  if (path2.IsEmpty())
  {
    m_pMainThread->GetCurrentPath(path2);
    if (path2.IsEmpty())
      return FZ_REPLY_INVALIDPARAM;
  }

  t_command command;
  command.id=FZ_COMMAND_DELETE;
  command.param1=FileName;
  command.path=path2;
  command.param4 = (filenameOnly ? 1 : 0);
  m_pMainThread->Command(command);
  return m_pMainThread->LastOperationSuccessful()?FZ_REPLY_OK:FZ_REPLY_ERROR;
}

int CFileZillaApi::RemoveDir(CString DirName, const CServerPath &path /*=CServerPath()*/)
{
  //Check if call allowed
  if (!m_bInitialized)
    return FZ_REPLY_NOTINITIALIZED;
  if (IsConnected()==FZ_REPLY_NOTCONNECTED)
    return FZ_REPLY_NOTCONNECTED;
  if (IsBusy()==FZ_REPLY_BUSY)
    return FZ_REPLY_BUSY;
  if (DirName==L"")
    return FZ_REPLY_INVALIDPARAM;

  CServerPath path2=path;
  if (path2.IsEmpty())
  {
    m_pMainThread->GetCurrentPath(path2);
    if (path2.IsEmpty())
      return FZ_REPLY_INVALIDPARAM;
  }

  t_command command;
  command.id=FZ_COMMAND_REMOVEDIR;
  command.param1=DirName;
  command.path=path2;
  m_pMainThread->Command(command);
  return m_pMainThread->LastOperationSuccessful()?FZ_REPLY_OK:FZ_REPLY_ERROR;
}

int CFileZillaApi::MakeDir(const CServerPath &path)
{
  //Check if call allowed
  if (!m_bInitialized)
    return FZ_REPLY_NOTINITIALIZED;
  if (IsConnected()==FZ_REPLY_NOTCONNECTED)
    return FZ_REPLY_NOTCONNECTED;
  if (IsBusy()==FZ_REPLY_BUSY)
    return FZ_REPLY_BUSY;
  if (path.IsEmpty() || !path.HasParent())
    return FZ_REPLY_INVALIDPARAM;

  t_command command;
  command.id=FZ_COMMAND_MAKEDIR;
  command.path=path;
  m_pMainThread->Command(command);
  return m_pMainThread->LastOperationSuccessful()?FZ_REPLY_OK:FZ_REPLY_ERROR;
}

int CFileZillaApi::Rename(CString oldName, CString newName, const CServerPath &path /*=CServerPath()*/, const CServerPath &newPath /*=CServerPath()*/)
{
  //Check if call allowed
  if (!m_bInitialized)
    return FZ_REPLY_NOTINITIALIZED;
  if (IsConnected()==FZ_REPLY_NOTCONNECTED)
    return FZ_REPLY_NOTCONNECTED;
  if (IsBusy()==FZ_REPLY_BUSY)
    return FZ_REPLY_BUSY;
  if (oldName==L"" || newName==L"")
    return FZ_REPLY_INVALIDPARAM;

  CServerPath path2 = path;
  if (path2.IsEmpty())
  {
    m_pMainThread->GetCurrentPath(path2);
    if (path2.IsEmpty())
      return FZ_REPLY_INVALIDPARAM;
  }

  t_command command;
  command.id = FZ_COMMAND_RENAME;
  command.param1 = oldName;
  command.param2 = newName;
  command.path = path2;
  command.newPath = newPath;
  m_pMainThread->Command(command);
  return m_pMainThread->LastOperationSuccessful()?FZ_REPLY_OK:FZ_REPLY_ERROR;
}

int CFileZillaApi::SetAsyncRequestResult(int nAction, CAsyncRequestData *pData)
{
  if (!pData)
    return FZ_REPLY_CRITICALERROR | FZ_REPLY_INVALIDPARAM;

  if (IsBadWritePtr(pData, sizeof(CAsyncRequestData)))
    return FZ_REPLY_CRITICALERROR;

  if (!m_bInitialized)
  {
    delete pData;
    return FZ_REPLY_NOTINITIALIZED;
  }
  if (IsConnected()==FZ_REPLY_NOTCONNECTED)
  {
    delete pData;
    return FZ_REPLY_NOTCONNECTED;
  }

  switch(pData->nRequestType)
  {
  case FZ_ASYNCREQUEST_OVERWRITE:
    break;
  case FZ_ASYNCREQUEST_VERIFYCERT:
    if (!((CVerifyCertRequestData *)pData)->pCertData)
    {
      delete pData;
      return FZ_REPLY_INVALIDPARAM;
    }
    break;
  case FZ_ASYNCREQUEST_NEEDPASS:
    break;
#ifndef MPEXT_NO_GSS
  case FZ_ASYNCREQUEST_GSS_AUTHFAILED:
  case FZ_ASYNCREQUEST_GSS_NEEDUSER:
  case FZ_ASYNCREQUEST_GSS_NEEDPASS:
    break;
#endif
  default:
    delete pData;
    return FZ_REPLY_INVALIDPARAM;
  }
  pData->nRequestResult = nAction;
  if (!m_pMainThread)
  {
    delete pData;
    return FZ_REPLY_NOTINITIALIZED;
  }

  m_pMainThread->PostThreadMessage(m_nInternalMessageID, FZAPI_THREADMSG_ASYNCREQUESTREPLY,  (LPARAM)pData);

  return FZ_REPLY_OK;
}

int CFileZillaApi::Chmod(int nValue, CString FileName, const CServerPath &path /*=CServerPath()*/ )
{
  //Check if call allowed
  if (!m_bInitialized)
    return FZ_REPLY_NOTINITIALIZED;
  if (IsConnected()==FZ_REPLY_NOTCONNECTED)
    return FZ_REPLY_NOTCONNECTED;
  if (IsBusy()==FZ_REPLY_BUSY)
    return FZ_REPLY_BUSY;
  if (FileName==L"")
    return FZ_REPLY_INVALIDPARAM;

  t_command command;
  command.id=FZ_COMMAND_CHMOD;
  command.param1=FileName;
  command.param4=nValue;
  command.path=path;
  m_pMainThread->Command(command);
  return m_pMainThread->LastOperationSuccessful()?FZ_REPLY_OK:FZ_REPLY_ERROR;
}

void CFileZillaApi::SetDebugLevel(int nDebugLevel)
{
  m_pMainThread->GetIntern()->SetDebugLevel(nDebugLevel);
}

//CAsyncRequestData derived classes
CAsyncRequestData::CAsyncRequestData()
{
  nRequestType = 0;
  nRequestID = 0;
  nRequestResult = 0;
}

CAsyncRequestData::~CAsyncRequestData()
{
}

COverwriteRequestData::COverwriteRequestData()
{
  size1 = 0;
  size2 = 0;
  nRequestType=FZ_ASYNCREQUEST_OVERWRITE;
  localtime=0;
  remotetime.hasdate = false;
  pTransferFile=0;
}

COverwriteRequestData::~COverwriteRequestData()
{
  delete pTransferFile;
  delete localtime;
}

CVerifyCertRequestData::CVerifyCertRequestData()
{
  nRequestType=FZ_ASYNCREQUEST_VERIFYCERT;
  pCertData=0;
}

CVerifyCertRequestData::~CVerifyCertRequestData()
{
  delete pCertData;
}

CNeedPassRequestData::CNeedPassRequestData()
{
  nRequestType=FZ_ASYNCREQUEST_NEEDPASS;
  nOldOpState=0;
}

CNeedPassRequestData::~CNeedPassRequestData()
{
}

#ifndef MPEXT_NO_GSS
CGssNeedPassRequestData::CGssNeedPassRequestData()
{
  nRequestType=FZ_ASYNCREQUEST_GSS_NEEDPASS;
}

CGssNeedPassRequestData::~CGssNeedPassRequestData()
{
}

CGssNeedUserRequestData::CGssNeedUserRequestData()
{
  nRequestType = FZ_ASYNCREQUEST_GSS_NEEDUSER;
}

CGssNeedUserRequestData::~CGssNeedUserRequestData()
{
}
#endif
