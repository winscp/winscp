//---------------------------------------------------------------------------
#include "FileZillaPCH.h"
//---------------------------------------------------------------------------
#include "FileZillaIntern.h"
#include "FileZillaIntf.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
TFileZillaIntern::TFileZillaIntern(TFileZillaIntf * AOwner) :
  FOwner(AOwner)
{
  FDebugLevel = 0;
}
//---------------------------------------------------------------------------
bool TFileZillaIntern::PostMessage(WPARAM wParam, LPARAM lParam) const
{
  bool Result;
  unsigned int MessageID = FZ_MSG_ID(wParam);

  switch (MessageID)
  {
    case FZ_MSG_STATUS:
    case FZ_MSG_ASYNCREQUEST:
    case FZ_MSG_LISTDATA:
    case FZ_MSG_TRANSFERSTATUS:
    case FZ_MSG_REPLY:
    case FZ_MSG_CAPABILITIES:
      Result = FOwner->PostMessage(wParam, lParam);
      break;

    default:
      DebugFail();
      Result = false;
      break;
  }

  return Result;
}
//---------------------------------------------------------------------------
CString TFileZillaIntern::GetOption(int OptionID) const
{
  return FOwner->Option(OptionID);
}
//---------------------------------------------------------------------------
int TFileZillaIntern::GetOptionVal(int OptionID) const
{
  return FOwner->OptionVal(OptionID);
}
//---------------------------------------------------------------------------
int TFileZillaIntern::GetDebugLevel() const
{
  return FDebugLevel;
}
//---------------------------------------------------------------------------
void TFileZillaIntern::SetDebugLevel(int DebugLevel)
{
  FDebugLevel = DebugLevel;
}
//---------------------------------------------------------------------------
