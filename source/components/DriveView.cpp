//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <System.DateUtils.hpp>
#include <System.IOUtils.hpp>
#include "CoreMain.h"
#include "DriveView.h"
#include "IEDriveInfo.h"
#include "DirView.h"
#ifdef DESIGN_ONLY
#undef AppLogFmt
#define AppLogFmt(S, F)
#endif
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
  TDateTime Start = Now();
  UnicodeString Path = NodePath(Node);
  TSearchRec SRec;
  if (!FindFirstSubDir(IncludeTrailingBackslash(Path) + L"*.*", SRec))
  {
    int Sec = static_cast<int>(SecondsBetween(Now(), Start));
    if (Sec > 2)
    {
      AppLogFmt(L"Took %d s to find that '%s' cannot be read", (Sec, Path));
    }
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
    if (!ReadSubDirsBatch(Node, SRec, CheckInterval, Limit, Start))
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
  auto NodeData = static_cast<TNodeData *>(FromNode->Data);
  return !NodeData->IsRecycleBin && !NodeData->IsRemote;
}
//---------------------------------------------------------------------------
void __fastcall TDriveView::AddChildNode(TTreeNode * ParentNode, UnicodeString ParentPath, const TSearchRec & SRec)
{
  auto NodeData = new TNodeData();
  NodeData->Attr = SRec.Attr;
  NodeData->DirName = SRec.Name;
  UnicodeString Path = IncludeTrailingBackslash(ParentPath) + SRec.Name;
  NodeData->IsRecycleBin =
    FLAGSET(SRec.Attr, faSysFile) &&
    (ParentNode->Parent == nullptr) &&
    (SameText(SRec.Name, L"RECYCLED") ||
     SameText(SRec.Name, L"RECYCLER") ||
     SameText(SRec.Name, L"$RECYCLE.BIN"));

  #ifndef DESIGN_ONLY
  if (FLAGSET(SRec.Attr, faSymLink))
  {
    String Target;
    NodeData->IsRemote =
      FileGetSymLinkTargetSafe(Path, Target) &&
      TPath::IsUNCRooted(Target);
  }
  #endif

  NodeData->Scanned = false;

  TTreeNode * NewNode = Items->AddChildObject(ParentNode, EmptyStr, NodeData);
  NewNode->Text = GetDisplayName(NewNode);
  NewNode->HasChildren = true;
  if ((GetDriveTypetoNode(ParentNode) != DRIVE_REMOTE) &&
      !NodeData->IsRemote)
  {
    FSubDirReaderThread->Add(NewNode, Path);
  }
}
