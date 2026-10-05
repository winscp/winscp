//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include "DriveView.h"
#include "IEDriveInfo.h"
#include "DirView.h"
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
//---------------------------------------------------------------------------
void __fastcall TDriveView::CreateWnd()
{
  #ifndef DESIGN_ONLY
  UnicodeString StartupSequenceTag = Name.SubString(1, 1);
  AddStartupSequence(L"Q" + StartupSequenceTag);
  #endif
  TDriveViewInt::CreateWnd();
  #ifndef DESIGN_ONLY
  AddStartupSequence(L"V" + StartupSequenceTag);
  #endif
}
//---------------------------------------------------------------------------
void __fastcall TDriveView::ReadSubDirs(TTreeNode * Node)
{
  auto NodeData = static_cast<TNodeData *>(Node->Data);
  UnicodeString Path = NodePath(Node);
  TSearchRec SRec;
  if (!FindFirstSubDir(IncludeTrailingBackslash(Path) + L"*.*", SRec))
  {
    Node->HasChildren = false;
  }
  else
  {
    int CheckInterval = 100;
    int Limit = DriveViewLoadingTooLongLimit * 1000;
    if (!Showing)
    {
      Limit /= 10;
      CheckInterval /= 10;
    }
    if (!ReadSubDirsBatch(Node, SRec, CheckInterval, Limit))
    {
      NodeData->DelayedSrec = SRec;
      NodeData->DelayedExclude = new TStringList();
      NodeData->DelayedExclude->CaseSensitive = false;
      NodeData->DelayedExclude->Sorted = true;
      FDelayedNodes->AddObject(Path, Node);
      DebugAssert(FDelayedNodes->Count < 20); // if more, something went likely wrong
      UpdateDelayedNodeTimer();
    }
    SortChildren(Node, false);
  }

  NodeData->Scanned = true;

  Application->ProcessMessages();
}
//---------------------------------------------------------------------------
bool __fastcall TDriveView::DoScanDir(TTreeNode * FromNode)
{
  return !static_cast<TNodeData *>(FromNode->Data)->IsRecycleBin;
}
//---------------------------------------------------------------------------
void __fastcall TDriveView::AddChildNode(TTreeNode * ParentNode, UnicodeString ParentPath, const TSearchRec & SRec)
{
  auto NodeData = new TNodeData();
  NodeData->Attr = SRec.Attr;
  NodeData->DirName = SRec.Name;
  NodeData->IsRecycleBin =
    FLAGSET(SRec.Attr, faSysFile) &&
    (ParentNode->Parent == nullptr) &&
    (SameText(SRec.Name, L"RECYCLED") ||
     SameText(SRec.Name, L"RECYCLER") ||
     SameText(SRec.Name, L"$RECYCLE.BIN"));
  NodeData->Scanned = false;

  TTreeNode * NewNode = Items->AddChildObject(ParentNode, EmptyStr, NodeData);
  NewNode->Text = GetDisplayName(NewNode);
  NewNode->HasChildren = true;
  if (GetDriveTypetoNode(ParentNode) != DRIVE_REMOTE)
  {
    FSubDirReaderThread->Add(NewNode, IncludeTrailingBackslash(ParentPath) + SRec.Name);
  }
}
