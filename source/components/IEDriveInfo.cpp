//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include "IEDriveInfo.h"
#include "PasTools.hpp"
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
//---------------------------------------------------------------------------
TDriveInfoRec * __fastcall TDriveInfo::AddDrive(UnicodeString Drive)
{
  TDriveInfoRec * Result = new TDriveInfoRec();
  FData.insert(std::make_pair(Drive, Result));
  ResetDrive(Drive);
  if (IsFixedDrive(Drive) || !IsRealDrive(Drive)) // not floppy
  {
    DoReadDriveStatus(Drive, dsSynchronous);
  }
  else
  {
    ReadDriveBasicStatus(Drive);
  }
  return Result;
}
//---------------------------------------------------------------------------
TDriveInfoRec * __fastcall TDriveInfo::GetInternal(UnicodeString Drive)
{
  auto I = FData.find(Drive);
  DebugAssert(I != FData.end());
  return I->second.get();
}
//---------------------------------------------------------------------------
TDriveInfoRec * __fastcall TDriveInfo::Get(UnicodeString Drive)
{
  NeedData();

  // We might want to wait for FReadyDrives to be empty before returning
  // (or even better do that only in DriveReady getter)
  TDriveInfoRec * Result;
  auto I = FData.find(Drive);
  if (I == FData.end())
  {
    DebugAssert(IsUncPath(Drive));
    Result = AddDrive(Drive);
    DriveRefresh();
  }
  else
  {
    Result = I->second.get();
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TDriveInfo::DriveRemoving(THandle DeviceHandle)
{
  for (auto I = FData.begin(); I != FData.end(); ++I)
  {
    if (I->second->DriveHandle == DeviceHandle)
    {
      TDriveInfoInt::DriveRemoving(I->first);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TDriveInfo::UpdateDrivesNotifications()
{
  for (auto I = FData.begin(); I != FData.end(); ++I)
  {
    UpdateDriveNotifications(I->first);
  }
}
