//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>

#include "UnixDriveView.h"

#ifndef DESIGN_ONLY
#include <Terminal.h>
#include <RemoteFiles.h>
#endif

#pragma package(smart_init)
//---------------------------------------------------------------------------
static inline void ValidCtrCheck(TUnixDriveView *)
{
  new TUnixDriveView(NULL);
}
//---------------------------------------------------------------------------
namespace Unixdriveview
{
  void __fastcall PACKAGE Register()
  {
    TComponentClass classes[1] = {__classid(TUnixDriveView)};
    RegisterComponents("Scp", classes, 0);
  }
}
//---------------------------------------------------------------------------
__fastcall TUnixDriveView::TUnixDriveView(TComponent * Owner) :
  TCustomUnixDriveView(Owner)
{
}
//---------------------------------------------------------------------------
struct TNodeData
{
  TRemoteFileList * FileList;
  TRemoteFile * File;
  AnsiString Directory;
};
//---------------------------------------------------------------------------
__fastcall TCustomUnixDriveView::TCustomUnixDriveView(TComponent* Owner) :
  TCustomDriveView(Owner)
{
  FTerminal = NULL;
  FRootName = DEFAULT_ROOTNAME;
  FIgnoreChange = false;
  FPrevSelected = NULL;
  DDAllowMove = false;
  FShowInaccesibleDirectories = true;
  FDummyDragFile = NULL;
}
//---------------------------------------------------------------------------
__fastcall TCustomUnixDriveView::~TCustomUnixDriveView()
{
  Terminal = NULL;
  if (FDummyDragFile != NULL)
  {
    #ifndef DESIGN_ONLY
    SAFE_DESTROY(FDummyDragFile);
    #endif
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomUnixDriveView::CreateWnd()
{
  TCustomDriveView::CreateWnd();

  FDragDropFilesEx->TargetEffects = TDropEffectSet() << deCopy << deMove;
}
//---------------------------------------------------------------------------
void __fastcall TCustomUnixDriveView::SetTerminal(TTerminal * value)
{
  if (FTerminal != value)
  {
    FTerminal = value;
    Items->Clear();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomUnixDriveView::SetDirView(TUnixDirView * Value)
{
  if (FDirView != Value)
  {
    if (FDirView != NULL)
    {
      FDirView->DriveView = NULL;
    }

    FDirView = Value;

    if (FDirView != NULL)
    {
      FDirView->DriveView = this;
    }
  }
}
//---------------------------------------------------------------------------
TCustomDirView * __fastcall TCustomUnixDriveView::GetCustomDirView()
{
  return FDirView;
}
//---------------------------------------------------------------------------
void __fastcall TCustomUnixDriveView::SetCustomDirView(TCustomDirView * Value)
{
  SetDirView(dynamic_cast<TUnixDirView *>(Value));
}
//---------------------------------------------------------------------------
bool __fastcall TCustomUnixDriveView::IsRootNameStored()
{
  return (FRootName != DEFAULT_ROOTNAME);
}
//---------------------------------------------------------------------------
bool __fastcall TCustomUnixDriveView::NodeIsHidden(const TTreeNode * Node)
{
  #ifndef DESIGN_ONLY
  TNodeData * Data = NodeData(Node);
  TRemoteFile * File = Data->File;
  return
    ((File != NULL) && File->IsHidden) ||
    ((File == NULL) && IsUnixHiddenFile(UnixExtractFileName(Data->Directory)));
  #else
  return false;
  #endif
}
//---------------------------------------------------------------------------
bool __fastcall TCustomUnixDriveView::NodeCanDelete(TTreeNode * Node)
{
  return (Selected == NULL) ||
    ((Selected != Node) && !Selected->HasAsParent(Node));
}
//---------------------------------------------------------------------------
void __fastcall TCustomUnixDriveView::UpdatePath(TTreeNode * Node, bool Force,
  bool CanLoad)
{
  #ifndef DESIGN_ONLY
  TNodeData * Data = NodeData(Node);
  AnsiString Path = Data->Directory;

  if (FTerminal->DirectoryFileList(Path, Data->FileList, CanLoad) ||
      ((Data->FileList != NULL) && Force))
  {
    TStringList * ChildrenDirs = new TStringList();
    try
    {
      ChildrenDirs->Sorted = true;
      ChildrenDirs->Duplicates = dupAccept;
      ChildrenDirs->CaseSensitive = true;

      bool HadChildren = (Node->Count > 0);
      if (HadChildren)
      {
        for (int i = 0; i < Node->Count; i++)
        {
          TTreeNode * ChildNode = Node->Item[i];
          TNodeData * ChildData = NodeData(ChildNode);
          ChildData->File = NULL;
          ChildrenDirs->AddObject(UnixExtractFileName(ChildData->Directory),
            ChildNode);
        }
      }

      for (int i = 0; i < Data->FileList->Count; i++)
      {
        TRemoteFile * File = Data->FileList->Files[i];
        if (File->IsDirectory && !File->IsParentDirectory && !File->IsThisDirectory &&
            (ShowHiddenDirs || !File->IsHidden) &&
            (ShowInaccesibleDirectories || !File->IsInaccesibleDirectory))
        {
          int ChildIndex = ChildrenDirs->IndexOf(File->FileName);
          if (ChildIndex >= 0)
          {
            TTreeNode * ChildNode =
              dynamic_cast<TTreeNode *>(ChildrenDirs->Objects[ChildIndex]);
            TNodeData * ChildData = NodeData(ChildNode);
            ChildData->File = File;
            UpdatePath(ChildNode, Force);
          }
          else
          {
            AnsiString ChildPath = UnixIncludeTrailingBackslash(Path) + File->FileName;
            assert(!IsUnixRootPath(ChildPath));

            LoadPathEasy(Node, ChildPath, File);
          }
        }
      }

      if (HadChildren)
      {
        for (int i = 0; i < ChildrenDirs->Count; i++)
        {
          TTreeNode * ChildNode = dynamic_cast<TTreeNode *>(ChildrenDirs->Objects[i]);
          TNodeData * ChildData = NodeData(ChildNode);
          if ((ChildData->File == NULL) && NodeCanDelete(ChildNode))
          {
            ChildNode->Delete();
          }
        }
      }

      Node->AlphaSort(false);
    }
    __finally
    {
      delete ChildrenDirs;
    }
  }
  else if (Force)
  {
    for (int i = Node->Count - 1; i >= 0; i--)
    {
      TTreeNode * ChildNode = Node->Item[i];
      TRemoteFile * File = NodeFile(ChildNode);
      if (!NodeCanDelete(ChildNode) ||
          ((ShowHiddenDirs || !NodeIsHidden(ChildNode)) &&
           (ShowInaccesibleDirectories || (File == NULL) || !File->IsInaccesibleDirectory)))
      {
        UpdatePath(ChildNode, true);
      }
      else
      {
        ChildNode->Delete();
      }
    }
  }
  #endif
}
//---------------------------------------------------------------------------
TTreeNode * __fastcall TCustomUnixDriveView::LoadPathEasy(TTreeNode * Parent,
  AnsiString Path, TRemoteFile * File)
{
  #ifndef DESIGN_ONLY
  assert(Path == UnixExcludeTrailingBackslash(Path));

  AnsiString DirName = IsUnixRootPath(Path) ? RootName : UnixExtractFileName(Path);
  TTreeNode * Node = Items->AddChild(Parent, DirName);

  TNodeData * Data = new TNodeData();
  Node->Data = Data;
  Data->Directory = Path;
  Data->File = File;
  Data->FileList = NULL;

  UpdatePath(Node, true);

  return Node;
  #else
  return NULL;
  #endif
}
//---------------------------------------------------------------------------
TTreeNode * __fastcall TCustomUnixDriveView::LoadPath(AnsiString Path)
{
  #ifndef DESIGN_ONLY

  if (Path.IsEmpty())
  {
    Path = ROOTDIRECTORY;
  }

  TTreeNode * Node = FindNodeToPath(Path);
  if (Node == NULL)
  {
    Path = UnixExcludeTrailingBackslash(Path);
    TTreeNode * Parent = NULL;
    TRemoteFile * File = NULL;

    if (!IsUnixRootPath(Path))
    {
      Parent = LoadPath(UnixExtractFileDir(Path));
    }

    Node = FindNodeToPath(Path);
    if (Node == NULL)
    {
      // node still does not exist, this should happen only when
      // if the parent directory is not in cache
      if (Parent != NULL)
      {
        TRemoteFileList * ParentFileList = NodeFileList(Parent);
        if (ParentFileList != NULL)
        {
          File = ParentFileList->FindFile(UnixExtractFileName(Path));
        }
      }

      Node = LoadPathEasy(Parent, Path, File);
      if (Parent != NULL)
      {
        Parent->AlphaSort(false);
      }
    }
    else
    {
      UpdatePath(Node, false);
    }
  }
  else
  {
    UpdatePath(Node, false);
  }

  return Node;
  #else
  return NULL;
  #endif
}
//---------------------------------------------------------------------------
void __fastcall TCustomUnixDriveView::LoadDirectory()
{
  #ifndef DESIGN_ONLY
  assert(!FIgnoreChange);
  FIgnoreChange = true;
  try
  {
    Selected = LoadPath(FTerminal->Files->Directory);
    assert(Selected != NULL);
    FPrevSelected = Selected;
  }
  __finally
  {
    FIgnoreChange = false;
    FDirectoryLoaded = true;
  }
  #endif
}
//---------------------------------------------------------------------------
TNodeData * __fastcall TCustomUnixDriveView::NodeData(const TTreeNode * Node)
{
  assert(Node->Data != NULL);

  return static_cast<TNodeData*>(Node->Data);
}
//---------------------------------------------------------------------------
TRemoteFileList * __fastcall TCustomUnixDriveView::NodeFileList(const TTreeNode * Node)
{
  assert(Node->Data != NULL);

  return static_cast<TNodeData*>(Node->Data)->FileList;
}
//---------------------------------------------------------------------------
TRemoteFile * __fastcall TCustomUnixDriveView::NodeFile(const TTreeNode * Node)
{
  assert(Node->Data != NULL);

  return static_cast<TNodeData*>(Node->Data)->File;
}
//---------------------------------------------------------------------------
TRemoteFile * __fastcall TCustomUnixDriveView::NodeFileForce(TTreeNode * Node)
{
  TRemoteFile * File = NodeFile(Node);

  if (File == NULL)
  {
    #ifndef DESIGN_ONLY
    SAFE_DESTROY(FDummyDragFile);
    FDummyDragFile = new TRemoteDirectoryFile();
    FDummyDragFile->FileName = Node->Text;
    FDummyDragFile->FullFileName = NodePathName(Node);
    File = FDummyDragFile;
    #endif
  }

  return File;
}
//---------------------------------------------------------------------------
void __fastcall TCustomUnixDriveView::Delete(TTreeNode * Node)
{
  TNodeData * Data = NULL;
  if (Node != NULL)
  {
    Data = NodeData(Node);
  }
  TCustomDriveView::Delete(Node);
  // delete file list at last, when we are sure that no child nodes exist
  if (Data != NULL)
  {
    #ifndef DESIGN_ONLY
    delete Data->FileList;
    #endif
    delete Data;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomUnixDriveView::Change(TTreeNode * Node)
{
  #ifndef DESIGN_ONLY
  // During D&D Selected is set to NULL and then back to previous selection,
  // prevent actually changing directory in such case
  if (FIgnoreChange || (Node == NULL) || (Node == FPrevSelected))
  {
    TCustomDriveView::Change(Node);
  }
  else
  {
    FDirectoryLoaded = false;
    try
    {
      Terminal->ChangeDirectory(NodePathName(Node));
      TCustomDriveView::Change(Node);
    }
    __finally
    {
      if (FDirectoryLoaded)
      {
        FPrevSelected = Selected;
      }
      else
      {
        assert(!FIgnoreChange);
        FIgnoreChange = true;
        try
        {
          Selected = FPrevSelected;
        }
        __finally
        {
          FIgnoreChange = false;
        }
      }
    }
  }
  #else
  TCustomDriveView::Change(Node);
  #endif
}
//---------------------------------------------------------------------------
void __fastcall TCustomUnixDriveView::PerformDragDropFileOperation(
  TTreeNode * Node, int Effect)
{
  if (OnDDFileOperation)
  {
    assert(DragDropFilesEx->FileList->Count > 0);
    assert(Node != NULL);

    AnsiString SourceDirectory;
    AnsiString TargetDirectory;

    SourceDirectory = ExtractFilePath(DragDropFilesEx->FileList->Items[0]->Name);
    TargetDirectory = NodeData(Node)->Directory;

    bool DoFileOperation = true;
    OnDDFileOperation(this, Effect, SourceDirectory, TargetDirectory,
      DoFileOperation);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomUnixDriveView::DDChooseEffect(int KeyState, int & Effect)
{
  if (DropTarget != NULL)
  {
    if ((KeyState & (MK_CONTROL | MK_SHIFT)) == 0)
    {
      Effect = DROPEFFECT_Copy;
    }
  }
  else
  {
    Effect = DROPEFFECT_None;
  }

  TCustomDriveView::DDChooseEffect(KeyState, Effect);
}
//---------------------------------------------------------------------------
void __fastcall TCustomUnixDriveView::UpdateDropTarget()
{
  // should never be NULL
  if (DropTarget != NULL)
  {
    UpdatePath(DropTarget, false, true);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomUnixDriveView::UpdateDropSource()
{
  // DragNode may be NULL if its parent directory was reloaded as result
  // of D&D operation and thus all child nodes are recreated 
  if ((DragNode != NULL) && (DragNode->Parent != NULL))
  {
    UpdatePath(DragNode->Parent, false, true);
  }
}
//---------------------------------------------------------------------------
TStrings * __fastcall TCustomUnixDriveView::DragFileList()
{
  assert(DragNode != NULL);
  TStrings * FileList = new TStringList();
  try
  {
    #ifndef DESIGN_ONLY
    FileList->AddObject(ExcludeTrailingBackslash(NodePathName(DragNode)),
      NodeFileForce(DragNode));
    #endif
  }
  catch(...)
  {
    delete FileList;
    throw;
  }
  return FileList;
}
//---------------------------------------------------------------------------
bool __fastcall TCustomUnixDriveView::DragCompleteFileList()
{
  return true;
}
//---------------------------------------------------------------------------
TDropEffectSet __fastcall TCustomUnixDriveView::DDSourceEffects()
{
  TDropEffectSet Result;
  Result << deCopy;
  if (DDAllowMove)
  {
    Result << deMove;
  }
  return Result;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TCustomUnixDriveView::NodePathName(TTreeNode * Node)
{
  // same as NodePath
  return NodeData(Node)->Directory;
}
//---------------------------------------------------------------------------
void __fastcall TCustomUnixDriveView::ClearDragFileList(TFileList * FileList)
{
  #ifndef DESIGN_ONLY
  if (FDummyDragFile != NULL)
  {
    SAFE_DESTROY(FDummyDragFile);
  }
  #endif
  TCustomDriveView::ClearDragFileList(FileList);
}
//---------------------------------------------------------------------------
void __fastcall TCustomUnixDriveView::AddToDragFileList(TFileList * FileList,
  TTreeNode * Node)
{
  AnsiString FileName = NodePathName(Node);
  TRemoteFile * File = NodeFileForce(Node);

  if (OnDDDragFileName != NULL)
  {
    OnDDDragFileName(this, File, FileName);
  }
  FileList->AddItem(NULL, FileName);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TCustomUnixDriveView::NodePath(TTreeNode * Node)
{
  // same as NodePathName
  return NodeData(Node)->Directory;
}
//---------------------------------------------------------------------------
bool __fastcall TCustomUnixDriveView::NodeIsRecycleBin(TTreeNode * /*Node*/)
{
  return false;
}
//---------------------------------------------------------------------------
bool __fastcall TCustomUnixDriveView::NodePathExists(TTreeNode * /*Node*/)
{
  return true;
}
//---------------------------------------------------------------------------
TColor __fastcall TCustomUnixDriveView::NodeColor(TTreeNode * Node)
{
  assert(Node != NULL);
  TColor Result = static_cast<TColor>(clDefaultItemColor);
  #ifndef DESIGN_ONLY
  if (FDimmHiddenDirs && !Node->Selected)
  {
    if (NodeIsHidden(Node))
    {
      Result = clGrayText;
    }
  }
  #endif
  return Result;
}
//---------------------------------------------------------------------------
Word __fastcall TCustomUnixDriveView::NodeOverlayIndexes(TTreeNode * Node)
{
#ifndef DESIGN_ONLY
  Word Result = oiNoOverlay;
  // Cannot query root node for file
  if (Node->Parent != NULL)
  {
    TRemoteFile * File = NodeFile(Node);
    if ((File != NULL) && (File->IsSymLink))
    {
      // broken link cannot probably happen anyway
      // as broken links are treated as files
      Result |= File->BrokenLink ? oiBrokenLink : oiLink;
    }
  }
  return Result;
#else
  return 0;
#endif
}
//---------------------------------------------------------------------------
void __fastcall TCustomUnixDriveView::GetImageIndex(TTreeNode * Node)
{
  TCustomDriveView::GetImageIndex(Node);
  Node->ImageIndex = StdDirIcon;
  Node->SelectedIndex = StdDirSelIcon;
}
//---------------------------------------------------------------------------
TTreeNode * __fastcall TCustomUnixDriveView::FindNodeToPath(AnsiString Path)
{
  TTreeNode * Result = NULL;
  #ifndef DESIGN_ONLY
  if (IsUnixRootPath(Path))
  {
    if (Items->Count > 0)
    {
      Result = Items->Item[0];
    }
  }
  else
  {
    Result = NULL;
    Path = UnixExcludeTrailingBackslash(Path);
    TTreeNode * Parent = NULL;
    if (!IsUnixRootPath(Path))
    {
      Parent = FindNodeToPath(UnixExtractFileDir(Path));
    }

    if ((Parent != NULL) && (Parent->Count > 0))
    {
      AnsiString DirName = UnixExtractFileName(Path);
      int StartIndex = 0;
      int EndIndex = Parent->Count - 1;

      while (true)
      {
        int Index = (StartIndex + EndIndex) / 2;
        AnsiString NodeDir = Parent->Item[Index]->Text;
        // lstrcmp is used by AlphaSort()
        int C = lstrcmp(DirName.c_str(), NodeDir.c_str());
        if (C == 0)
        {
          Result = Parent->Item[Index];
          break;
        }
        else if (C < 0)
        {
          if (Index == StartIndex)
          {
            break;
          }
          EndIndex = Index - 1;
        }
        else
        {
          if (Index == EndIndex)
          {
            break;
          }
          StartIndex = Index + 1;
        }
      }
    }
  }
  #endif
  return Result;
}
//---------------------------------------------------------------------------
TTreeNode * __fastcall TCustomUnixDriveView::FindPathNode(AnsiString Path)
{
  TTreeNode * Result = NULL;

  #ifndef DESIGN_ONLY
  if (Items->Count > 0)
  {
    do
    {
      Result = FindNodeToPath(Path);
      if (Result == NULL)
      {
        assert(!IsUnixRootPath(Path));
        Path = UnixExtractFileDir(UnixExcludeTrailingBackslash(Path));
      }
    }
    while (Result == NULL);
  }
  #endif

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomUnixDriveView::ValidateDirectoryEx(TTreeNode * /*Node*/,
  TRecursiveScan /*Recurse*/, bool /*NewDirs*/)
{
  // nothing
}
//---------------------------------------------------------------------------
void __fastcall TCustomUnixDriveView::RebuildTree()
{
  if (Items->Count > 0)
  {
    UpdatePath(Items->Item[0], true);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomUnixDriveView::SetShowInaccesibleDirectories(bool value)
{
  if (FShowInaccesibleDirectories != value)
  {
    FShowInaccesibleDirectories = value;
    RebuildTree();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomUnixDriveView::CMShowingChanged(TMessage & Message)
{
  TCustomDriveView::Dispatch(&Message);
  if (Showing && (Terminal != NULL))
  {
    LoadDirectory();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomUnixDriveView::DisplayContextMenu(TTreeNode * /*Node*/,
  const TPoint & /*ScreenPos*/)
{
  // TODO
}
//---------------------------------------------------------------------------
void __fastcall TCustomUnixDriveView::DisplayPropertiesMenu(TTreeNode * /*Node*/)
{
  // TODO
}

