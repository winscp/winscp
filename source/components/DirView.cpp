//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "DirView.h"
#include "IEDriveInfo.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
__fastcall TDirView::TDirView(TComponent * AOwner) :
  TDirViewInt(AOwner)
{
  DriveInfoRequire();
}
