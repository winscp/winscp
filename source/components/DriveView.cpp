//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include "DriveView.h"
#include "IEDriveInfo.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
__fastcall TDriveView::TDriveView(TComponent * AOwner) :
  TDriveViewInt(AOwner)
{
  DriveInfoRequire();

  FDriveStatus.reset(new TStringList());
  FDriveStatus->Sorted = true;
  for (TRealDrive Drive = FirstDrive; Drive <= LastDrive; Drive++)
  {
    FDriveStatus->AddObject(Drive, CreateDriveStatus());
  }
}
//---------------------------------------------------------------------------
TDriveStatus * __fastcall TDriveView::GetDriveStatus(UnicodeString Drive)
{
  int Index = FDriveStatus->IndexOf(Drive);
  TDriveStatus * Result;
  if (Index < 0)
  {
    Result = CreateDriveStatus();
    FDriveStatus->AddObject(Drive, Result);
  }
  else
  {
    Result = GetDriveStatus(Index);
  }
  return Result;
}
//---------------------------------------------------------------------------
TDriveStatus * TDriveView::GetDriveStatus(int Index)
{
  return DebugNotNull(dynamic_cast<TDriveStatus *>(FDriveStatus->Objects[Index]));
}
//---------------------------------------------------------------------------
bool __fastcall TDriveView::GetNextDriveStatus(int & Iterator, UnicodeString * Drive, TDriveStatus *& Status)
{
  bool Result = (Iterator < FDriveStatus->Count);
  if (Result)
  {
    if (Drive != nullptr)
    {
      *Drive = FDriveStatus->Strings[Iterator];
    }
    Status = GetDriveStatus(Iterator);
    ++Iterator;
  }
  return Result;
}
