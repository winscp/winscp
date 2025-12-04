//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "DriveView.h"
#include "IEDriveInfo.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
__fastcall TDriveView::TDriveView(TComponent * AOwner) :
  TDriveViewInt(AOwner)
{
  DriveInfoRequire();
}
