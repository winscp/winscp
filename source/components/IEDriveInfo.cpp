//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "IEDriveInfo.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
void DriveInfoRequire()
{
  if (DriveInfo == nullptr)
  {
    DriveInfoInit(new TDriveInfo());
  }
}
//---------------------------------------------------------------------------
__fastcall TDriveInfo::TDriveInfo() :
  TDriveInfoInt()
{
}
