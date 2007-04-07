//!!!
//---------------------------------------------------------------------------
#include "stdafx.h"
//---------------------------------------------------------------------------
#include "FileZillaIntern.h"
#include "FileZillaIntf.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
TFileZillaIntern::TFileZillaIntern(TFileZillaIntf * AOwner) :
  FOwner(AOwner)
{
  // not being initialied by CApiLog
  m_nLogMessage = 0;
}
//---------------------------------------------------------------------------
BOOL TFileZillaIntern::PostMessage(HWND hWnd, UINT Msg, WPARAM wParam,
  LPARAM lParam) const
{
  ASSERT(hWnd == NULL);
  ASSERT(Msg == 0);

  bool Result;
  unsigned int MessageID = FZ_MSG_ID(wParam);
  
  switch (MessageID)
  {
    // TODO !!! separated temporarily to ease debugging  
    case FZ_MSG_STATUS:
      Result = FOwner->PostMessage(wParam, lParam);
      break;

    case FZ_MSG_ASYNCREQUEST:  
      Result = FOwner->PostMessage(wParam, lParam);
      break;

    case FZ_MSG_LISTDATA:
      Result = FOwner->PostMessage(wParam, lParam);
      break;

    case FZ_MSG_TRANSFERSTATUS:  
      Result = FOwner->PostMessage(wParam, lParam);
      break;

    case FZ_MSG_REPLY:
      Result = FOwner->PostMessage(wParam, lParam);
      break;

    // ignored for performace
    case FZ_MSG_SOCKETSTATUS:
      Result = false;
      break;

    // should never get here, no GSS/SSL support, only on close the secure-off is reported
    case FZ_MSG_SECURESERVER:
      ASSERT(FZ_MSG_PARAM(wParam) == FALSE);
      ASSERT(lParam == 0);
      Result = false;
      break;
    
    // should never get here, call compiled out in filezilla code
    case FZ_MSG_QUITCOMPLETE:
    default:
      ASSERT(FALSE);
      Result = false;
      break;
  }
      
  return (Result ? TRUE : FALSE);
}
//---------------------------------------------------------------------------
