//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
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
//---------------------------------------------------------------------------
void __fastcall TDirView::IconUpdateClear()
{
  FIconUpdateQueue = TIconUpdateQueue();
  FIconUpdateQueueDeferred = TIconUpdateQueue();
  FIconUpdateSet.clear();
}
//---------------------------------------------------------------------------
void __fastcall TDirView::IconUpdateEnqueue(TListItem * ListItem)
{
  if (FIconUpdateSet.find(ListItem->Index) == FIconUpdateSet.end())
  {
    FIconUpdateSet.insert(ListItem->Index);
    TIconUpdateSchedule Schedule { .Index = ListItem->Index };
    FIconUpdateQueue.push(Schedule);
    DebugAssert(FIconUpdateSet.size() == FIconUpdateQueue.size() + FIconUpdateQueueDeferred.size());
  }
  StartIconUpdateThread();
}
//---------------------------------------------------------------------------
int __fastcall TDirView::IconUpdatePeek()
{
  int Result;
  if (!FIconUpdateQueue.empty())
  {
    Result = FIconUpdateQueue.front().Index;
  }
  else if (!FIconUpdateQueueDeferred.empty())
  {
    Result = FIconUpdateQueueDeferred.front().Index;
  }
  else
  {
    Result = -1;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TDirView::IconUpdateDeprioritize(Dirviewint::TFileRec * ItemData, int Index)
{
  bool Result = !FIconUpdateQueue.empty();
  if (Result)
  {
    DebugAssert(!ThumbnailNeeded(ItemData));
    if (ItemData->IconEmpty)
    {
      TIconUpdateSchedule Schedule = FIconUpdateQueue.front();
      FIconUpdateQueue.pop();
      FIconUpdateQueueDeferred.push(Schedule);
    }
    else
    {
      IconUpdateDequeue(Index);
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TDirView::IconUpdateDequeue(int Index)
{
  if (!FIconUpdateQueue.empty())
  {
    FIconUpdateQueue.pop();
  }
  else
  {
    FIconUpdateQueueDeferred.pop();
  }
  FIconUpdateSet.erase(Index);
  DebugAssert(FIconUpdateSet.size() == FIconUpdateQueue.size() + FIconUpdateQueueDeferred.size());
}
//---------------------------------------------------------------------------
void __fastcall TDirView::PathChanged()
{
  TDirViewInt::PathChanged();
  // make sure to use PathName as Path maybe just X: what
  // ExpandFileName resolves to current working directory
  // on the drive, not to root path
  UnicodeString Expanded = ExpandFileName(PathName);
  FLastPaths[DriveInfo->GetDriveKey(Expanded)] = Expanded;
}
//---------------------------------------------------------------------------
bool __fastcall TDirView::TryGetLastPath(UnicodeString Drive, UnicodeString & Path)
{
  auto I = FLastPaths.find(Drive);
  bool Result = (I != FLastPaths.end());
  if (Result)
  {
    Path = I->second;
    #ifndef DESIGN_ONLY
    if (!DirectoryExists(ApiPath(Path)))
    {
      if (DriveInfo->IsRealDrive(Drive))
      {
        Path = FORMAT(L"%s:", (Drive));
      }
      else
      {
        Path = Drive;
      }
    }
    #endif
  }
  return Result;
}
