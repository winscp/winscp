//---------------------------------------------------------------------------
#include "FileZillaPCH.h"
#include "ApiLog.h"

//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

CApiLog::CApiLog()
{
  FIntern = NULL;
}

CApiLog::~CApiLog()
{
}

void CApiLog::InitIntern(TFileZillaIntern * Intern)
{
  FIntern = Intern;
}

TFileZillaIntern * CApiLog::GetIntern()
{
  return FIntern;
}

bool CApiLog::LoggingMessageType(int nMessageType) const
{
  return
    (nMessageType < FZ_LOG_APIERROR) ||
    ((nMessageType-FZ_LOG_APIERROR) < FIntern->GetDebugLevel());
}

void CApiLog::LogMessage(int nMessageType, LPCTSTR pMsgFormat, ...) const
{
  DebugAssert(nMessageType>=FZ_LOG_STATUS && nMessageType<=FZ_LOG_DEBUG);
  if (!LoggingMessageType(nMessageType))
    return;

  va_list ap;

  va_start(ap, pMsgFormat);
  CString text;
  text.FormatV(pMsgFormat, ap);
  va_end(ap);

  if (nMessageType>=FZ_LOG_DEBUG)
    return;
  SendLogMessage(nMessageType, text);
}

void CApiLog::LogMessageRaw(int nMessageType, LPCTSTR pMsg) const
{
  DebugAssert(nMessageType>=FZ_LOG_STATUS && nMessageType<=FZ_LOG_DEBUG);
  if (!LoggingMessageType(nMessageType))
    return;

  if (nMessageType>=FZ_LOG_DEBUG)
    return;
  SendLogMessage(nMessageType, pMsg);
}

void CApiLog::SendLogMessage(int nMessageType, LPCTSTR pMsg) const
{
  if (!LoggingMessageType(nMessageType))
    return;
  //Displays a message in the message log
  t_ffam_statusmessage *pStatus = new t_ffam_statusmessage;
  pStatus->post = TRUE;
  pStatus->status = pMsg;
  pStatus->type = nMessageType;
  if (!FIntern->PostMessage(FZ_MSG_MAKEMSG(FZ_MSG_STATUS, 0), (LPARAM)pStatus))
    delete pStatus;
}

CString CApiLog::GetOption(int OptionID) const
{
  DebugAssert(FIntern != NULL);
  return FIntern->GetOption(OptionID);
}

int CApiLog::GetOptionVal(int OptionID) const
{
  DebugAssert(FIntern != NULL);
  return FIntern->GetOptionVal(OptionID);
}

void CApiLog::LogError(int Error)
{
  wchar_t * Buffer;
  int Len = FormatMessage(
    FORMAT_MESSAGE_FROM_SYSTEM |
    FORMAT_MESSAGE_IGNORE_INSERTS |
    FORMAT_MESSAGE_ARGUMENT_ARRAY |
    FORMAT_MESSAGE_ALLOCATE_BUFFER, NULL, Error, 0, (LPTSTR)&Buffer, 0, NULL);
  if (Len > 0)
  {
    LogMessageRaw(FZ_LOG_ERROR, Buffer);
    LocalFree(Buffer);
  }
}
