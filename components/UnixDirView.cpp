//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>

#include "UnixDirView.h"
#include "UnixDriveView.h"

#include <FileCtrl.hpp>

#ifndef DESIGN_ONLY
#include <ScpMain.h>
#include <Terminal.h>
#include <WinConfiguration.h>
#endif

#pragma package(smart_init)
#ifndef DESIGN_ONLY
#define ITEMFILE ((TRemoteFile *)(Item->Data))
#define ASSERT_VALID_ITEM assert(Item && Item->Data && Terminal); assert(Terminal->Files->IndexOf(ITEMFILE) >= 0)
#endif
//---------------------------------------------------------------------------
static inline void ValidCtrCheck(TUnixDirView *)
{
  new TUnixDirView(NULL);
}
//---------------------------------------------------------------------------
namespace Unixdirview
{
  void __fastcall PACKAGE Register()
  {
    TComponentClass classes[1] = {__classid(TUnixDirView)};
    RegisterComponents("Scp", classes, 0);
  }
}
//---------------------------------------------------------------------------
#ifndef DESIGN_ONLY
#define RFILE(N) ((TRemoteFile *)(Item ## N->Data))
int __stdcall CompareDirectories(TListItem *Item1, TListItem *Item2, TUnixDirView *DirView)
{
  // Because CompareDirectories is called from each other compare functions
  // it's sufficient to check pointers only here (see below)
  assert(DirView && Item1 && RFILE(1) && Item2 && RFILE(2));

  if (RFILE(1)->IsParentDirectory && !RFILE(2)->IsParentDirectory) return -1;
    else
  if (!RFILE(1)->IsParentDirectory && RFILE(2)->IsParentDirectory) return 1;
    else
  if (DirView->DirsOnTop &&
      RFILE(1)->IsDirectory && !RFILE(2)->IsDirectory) return -1;
    else
  if (DirView->DirsOnTop &&
      !RFILE(1)->IsDirectory && RFILE(2)->IsDirectory) return 1;
    else
  return 0;
}
//---------------------------------------------------------------------------
#define DEFINE_COMPARE_FUNC(Property, CompareFunc) \
  int __stdcall Compare ## Property(TListItem *Item1, TListItem *Item2, TUnixDirView *DirView) \
  { int Result = CompareDirectories(Item1, Item2, DirView); \
    if (!Result) { Result = CompareFunc(RFILE(1)->Property, RFILE(2)->Property); \
      if (!DirView->UnixColProperties->SortAscending) Result = -Result; }\
    return Result; }
#define COMPARE_NUMBER(Num1, Num2) ( Num1 < Num2 ? -1 : ( Num1 > Num2 ? 1 : 0) )
//---------------------------------------------------------------------------
DEFINE_COMPARE_FUNC(FileName, AnsiCompareText);
DEFINE_COMPARE_FUNC(Size, COMPARE_NUMBER);
DEFINE_COMPARE_FUNC(Modification, COMPARE_NUMBER);
DEFINE_COMPARE_FUNC(RightsStr, AnsiCompareText);
DEFINE_COMPARE_FUNC(Owner, AnsiCompareText);
DEFINE_COMPARE_FUNC(Group, AnsiCompareText);
DEFINE_COMPARE_FUNC(Extension, AnsiCompareText);
//---------------------------------------------------------------------------
#undef DEFINE_COMPARE_FUNC
#undef COMPARE_NUMBER
#undef RFILE
#endif
//---------------------------------------------------------------------------
#define HOMEDIRECTORY ""
//---------------------------------------------------------------------------
__fastcall TUnixDirView::TUnixDirView(TComponent* Owner)
        : TCustomUnixDirView(Owner)
{
#ifndef DESIGN_ONLY
  FTerminal = NULL;
#endif
  FCaseSensitive = true;
  DDAllowMove = false;
  FShowInaccesibleDirectories = true;
  FFullLoad = false;
  FDriveView = NULL;
}
//---------------------------------------------------------------------------
__fastcall TUnixDirView::~TUnixDirView()
{
#ifndef DESIGN_ONLY
  Terminal = NULL;
#endif
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::DisplayContextMenu(const TPoint &Where)
{
  bool Handled = false;
  if (OnContextPopup) OnContextPopup(this, ScreenToClient(Where), Handled);
  if (!Handled)
  {
    if (PopupMenu && !PopupMenu->AutoPopup)
      PopupMenu->Popup(Where.x, Where.y);
  }
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::DisplayPropertiesMenu()
{
  if (OnDisplayProperties) OnDisplayProperties(this);
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::ExecuteFile(TListItem * Item)
{
#ifndef DESIGN_ONLY
  ASSERT_VALID_ITEM;
  if (ITEMFILE->IsDirectory)
  {
    FLastPath = PathName;
    ChangeDirectory(ITEMFILE->FileName);
  }
  else
  {
    if (ItemFocused != Item) ItemFocused = Item;
    DisplayPropertiesMenu();
  }
#endif
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::ExecuteParentDirectory()
{
  // We need to remember this to select directory being leaved in parent directory
  FLastPath = PathName;
#ifndef DESIGN_ONLY
  ChangeDirectory(PARENTDIRECTORY);
#endif
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::ExecuteHomeDirectory()
{
#ifndef DESIGN_ONLY
  // don't select any directory
  FLastPath = "";
  AnsiString APath = Terminal->SessionData->RemoteDirectory;
  if (WinConfiguration->DefaultDirIsHome && !APath.IsEmpty() &&
      !Terminal->SessionData->UpdateDirectories)
  {
    if (APath[1] != '/')
    {
      Terminal->BeginTransaction();
      try
      {
        ChangeDirectory(HOMEDIRECTORY);
        ChangeDirectory(APath);
      }
      __finally
      {
        Terminal->EndTransaction();
      }
    }
    else
    {
      ChangeDirectory(APath);
    }
  }
  else
  {
    ChangeDirectory(HOMEDIRECTORY);
  }
#endif
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::ReloadDirectory()
{
#ifndef DESIGN_ONLY
  FLastPath = "";
  DoAnimation(true);
  Terminal->ReloadDirectory();
#endif
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::ExecuteRootDirectory()
{
#ifndef DESIGN_ONLY
  // We set LastPath to top directory, so it will be selected
  // after entering root directory
  // DISABLED: see PathChanged(): back moves to top directory, not to current

  FLastPath = PathName;
  ChangeDirectory(ROOTDIRECTORY);
#endif
}
//---------------------------------------------------------------------------
bool __fastcall TUnixDirView::ItemIsDirectory(TListItem * Item)
{
#ifndef DESIGN_ONLY
  ASSERT_VALID_ITEM;
  return ITEMFILE->IsDirectory;
#else
  return false;
#endif
}
//---------------------------------------------------------------------------
bool __fastcall TUnixDirView::ItemIsFile(TListItem * Item)
{
#ifndef DESIGN_ONLY
  ASSERT_VALID_ITEM;
  return !(ITEMFILE->IsParentDirectory);
#else
  return false;
#endif
}
//---------------------------------------------------------------------------
bool __fastcall TUnixDirView::ItemIsParentDirectory(TListItem * Item)
{
#ifndef DESIGN_ONLY
  ASSERT_VALID_ITEM;
  return ITEMFILE->IsParentDirectory;
#else
  return false;
#endif
}
//---------------------------------------------------------------------------
AnsiString __fastcall TUnixDirView::ItemFileName(TListItem * Item)
{
#ifndef DESIGN_ONLY
  ASSERT_VALID_ITEM;
  return ITEMFILE->FileName;
#else
  return AnsiString();
#endif
}
//---------------------------------------------------------------------------
__int64 __fastcall TUnixDirView::ItemFileSize(TListItem * Item)
{
#ifndef DESIGN_ONLY
  ASSERT_VALID_ITEM;
  return ITEMFILE->IsDirectory ? 0 : ITEMFILE->Size;
#else
  return 0;
#endif
}
//---------------------------------------------------------------------------
AnsiString __fastcall TUnixDirView::ItemFullFileName(TListItem * Item)
{
#ifndef DESIGN_ONLY
  ASSERT_VALID_ITEM;
  return ITEMFILE->FullFileName;
#else
  return AnsiString();
#endif
}
//---------------------------------------------------------------------------
int __fastcall TUnixDirView::ItemImageIndex(TListItem * Item, bool /*Cache*/)
{
#ifndef DESIGN_ONLY
  ASSERT_VALID_ITEM;
  // TCustomDirView::ItemImageIndex is used for icon caching
  // so we don't need it here. But it's implemented anyway.
  return ITEMFILE->IconIndex;
#else
  return 0;
#endif
}
//---------------------------------------------------------------------------
bool __fastcall TUnixDirView::ItemMatchesFilter(TListItem * Item,
  const TFileFilter &Filter)
{
#ifndef DESIGN_ONLY
  ASSERT_VALID_ITEM;
  TRemoteFile *File = ITEMFILE;
  int Attr = File->Attr;

  return
    ((Attr & Filter.IncludeAttr) == Filter.IncludeAttr) &&
    ((Attr & Filter.ExcludeAttr) == 0) &&
    ((!File->IsDirectory) || Filter.Directories) &&
    ((Filter.FileSizeFrom == 0) || (File->Size >= Filter.FileSizeFrom)) &&
    ((Filter.FileSizeTo == 0) || (File->Size <= Filter.FileSizeTo)) &&
    ((!(int)Filter.ModificationFrom) || (File->Modification >= Filter.ModificationFrom)) &&
    ((!(int)Filter.ModificationTo) || (File->Modification <= Filter.ModificationTo)) &&
    ((Filter.Masks.IsEmpty()) ||
     FileNameMatchesMasks(File->FileName, Filter.Masks));
#else
  return false;
#endif
}
//---------------------------------------------------------------------------
Word __fastcall TUnixDirView::ItemOverlayIndexes(TListItem * Item)
{
#ifndef DESIGN_ONLY
  ASSERT_VALID_ITEM;
  Word Result = oiNoOverlay;
  if (ITEMFILE->IsParentDirectory)
  {
    Result |= oiDirUp;
  }
  if (ITEMFILE->IsSymLink)
  {
    Result |= ITEMFILE->BrokenLink ? oiBrokenLink : oiLink;
  }
  return Result;
#else
  return 0;
#endif
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::LoadFiles()
{
#ifndef DESIGN_ONLY
  assert(Terminal);
  if (DirOK)
  {
    // it's enough if we reach this point, we don't require that loading files into
    // list succeeded. FDirLoadedAfterChangeDir == false tells only that
    // loding file list from server failed, not loading into listview
    FDirLoadedAfterChangeDir = true;

    FFilesSize = 0;
    FHasParentDir = false;
    int VisibleFiles = 0;
    for (int Index = 0; Index < Terminal->Files->Count; Index++)
    {
      TRemoteFile *File = Terminal->Files->Files[Index];
      assert(File);
      TListItem *Item;
      if ((ShowHiddenFiles || !File->IsHidden) &&
          (ShowInaccesibleDirectories || !File->IsInaccesibleDirectory))
      {
        VisibleFiles++;

        if (!File->IsDirectory) FFilesSize += File->Size;
        if (File->IsParentDirectory) FHasParentDir = true;

        Item = Items->Add();
        Item->Data = File;
        Item->Caption = File->FileName;
        if (FFullLoad)
        {
          Item->ImageIndex = File->IconIndex;
          Item->SubItems->Add((!File->IsDirectory ? FormatFloat("#,##0", File->Size) : AnsiString()));
          Item->SubItems->Add(File->UserModificationStr);
          Item->SubItems->Add(File->RightsStr);
          Item->SubItems->Add(File->Owner);
          Item->SubItems->Add(File->Group);
          Item->SubItems->Add(File->Extension);
        }
      }
    }

    if (OwnerData)
    {
      Items->Count = VisibleFiles;
    }
  }
#endif
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::GetDisplayInfo(TListItem * Item, tagLVITEMA &DispInfo)
{
  if (!FFullLoad)
  {
#ifndef DESIGN_ONLY
    TRemoteFile * File = ITEMFILE;
    if (DispInfo.mask & LVIF_TEXT)
    {
      AnsiString Value;
      switch (DispInfo.iSubItem) {
        case uvName: Value = File->FileName; break;
        case uvSize: Value = (!File->IsDirectory ? FormatFloat("#,##0", File->Size) : AnsiString()); break;
        case uvChanged: Value = File->UserModificationStr; break;
        case uvRights: Value = File->RightsStr; break;
        case uvOwner: Value = File->Owner; break;
        case uvGroup: Value = File->Group; break;
        case uvExt: Value = File->Extension; break;
        default: assert(false);
      }
      StrPLCopy(DispInfo.pszText, Value, DispInfo.cchTextMax);
    }

    if (DispInfo.iSubItem == 0 && DispInfo.mask & LVIF_IMAGE)
    {
      DispInfo.iImage = File->IconIndex;
      DispInfo.mask |= LVIF_DI_SETITEM;
    }
#endif
  }
}
//---------------------------------------------------------------------------
bool __fastcall TUnixDirView::PasteFromClipBoard(AnsiString TargetPath)
{
  DragDropFilesEx->FileList->Clear();
  bool Result = false;
  if (CanPasteFromClipBoard() &&
      DragDropFilesEx->GetFromClipboard())
  {
    if (TargetPath.IsEmpty())
    {
      TargetPath = PathName;
    }

    PerformItemDragDropOperation(NULL, DROPEFFECT_COPY);
    if (OnDDExecuted != NULL)
    {
      OnDDExecuted(this, DROPEFFECT_COPY);
    }
    Result = true;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::PerformItemDragDropOperation(TListItem * Item,
  int Effect)
{
#ifndef DESIGN_ONLY
  if (OnDDFileOperation)
  {
    assert(DragDropFilesEx->FileList->Count > 0);

    AnsiString SourceDirectory;
    AnsiString TargetDirectory;

    SourceDirectory = ExtractFilePath(DragDropFilesEx->FileList->Items[0]->Name);
    if (Item)
    {
      assert(ITEMFILE->IsDirectory && (Terminal->Files->IndexOf(ITEMFILE) >= 0));
      TargetDirectory = ITEMFILE->FullFileName;
    }
    else
    {
      TargetDirectory = Path;
    }

    bool DoFileOperation = true;
    OnDDFileOperation(this, Effect, SourceDirectory, TargetDirectory,
      DoFileOperation);
  }
#endif
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::SetItemImageIndex(TListItem * /* Item */, int /* Index */)
{
  // TCustomDirView::SetItemImageIndex is used for icon caching
  // so we don't need it here.
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::DDMenuDone(TObject* /* Sender */, HMENU /* AMenu */)
{
  // TODO: Why I need to duplicate this method? (see TCustomDirView::DDMenuDone)
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::SetDriveView(TCustomUnixDriveView * Value)
{
  if (Value != FDriveView)
  {
    if (FDriveView != NULL)
    {
      FDriveView->Terminal = NULL;
    }

    FDriveView = Value;

    if (FDriveView != NULL)
    {
      FDriveView->Terminal = Terminal;
    }
  }
}
//---------------------------------------------------------------------------
#ifndef DESIGN_ONLY
void __fastcall TUnixDirView::SetTerminal(TTerminal *value)
{
  if (FTerminal != value)
  {
    if (FTerminal)
    {
      assert(FTerminal->OnReadDirectory == DoReadDirectory);
      Terminal->OnReadDirectory = NULL;
      assert(FTerminal->OnChangeDirectory == DoChangeDirectory);
      FTerminal->OnChangeDirectory = NULL;
      if (!value || !value->Files->Loaded)
      {
        ClearItems();
      }
    }
    FTerminal = value;
    if (FDriveView != NULL)
    {
      FDriveView->Terminal = FTerminal;
    }
    if (FTerminal)
    {
      FTerminal->OnReadDirectory = DoReadDirectory;
      FTerminal->OnChangeDirectory = DoChangeDirectory;
      FTerminal->Files->IncludeParentDirectory = AddParentDir;
      if (FTerminal->Files->Loaded)
      {
        DoChangeDirectory(FTerminal);
        DoReadDirectory(FTerminal, false);
      }
    }
  }
}
#endif
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::DoReadDirectory(TObject * /*Sender*/, bool ReloadOnly)
{
#ifndef DESIGN_ONLY
  if (Terminal->Active)
  {
    if (ReloadOnly)
    {
      Reload(false);
    }
    else
    {
      Load();
      PathChanged();
    }

    if ((FDriveView != NULL) && FDriveView->Visible)
    {
      FDriveView->LoadDirectory();
    }
  }
  else
  {
    // Make sure file list is cleared, to remove all references to invalid
    // file objects. LoadFiles check for disconnected terminal, so no reloading
    // actually occures.
    Load();
  }
#endif
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::DoChangeDirectory(TObject * /*Sender*/)
{
#ifndef DESIGN_ONLY
//  Reload(false);
#endif
}
//---------------------------------------------------------------------------
bool __fastcall TUnixDirView::GetDirOK()
{
#ifndef DESIGN_ONLY
  return (Active && Terminal->Files->Loaded);
#else
  return false;
#endif
}
//---------------------------------------------------------------------------
AnsiString __fastcall TUnixDirView::GetPathName()
{
#ifndef DESIGN_ONLY
  if (DirOK) return Terminal->CurrentDirectory;
    else
#endif
  return "";
}
//---------------------------------------------------------------------------
AnsiString __fastcall TUnixDirView::GetPath()
{
#ifndef DESIGN_ONLY
  if (DirOK) return UnixIncludeTrailingBackslash(Terminal->CurrentDirectory);
    else
#endif
  return "";
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::SetPath(AnsiString Value)
{
#ifndef DESIGN_ONLY
  Value = UnixExcludeTrailingBackslash(
    StringReplace(Value, '\\', '/', TReplaceFlags() << rfReplaceAll));

  if (Active && (Terminal->CurrentDirectory != Value))
  {
    FLastPath = PathName;
    Terminal->CurrentDirectory = Value;
  }
#endif
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::SortItems()
{
#ifndef DESIGN_ONLY
  assert(Terminal);
  if (HandleAllocated())
  {
    PFNLVCOMPARE SortProc;
    switch (SortColumn) {
      case uvName: SortProc = (PFNLVCOMPARE)CompareFileName; break;
      case uvSize: SortProc = (PFNLVCOMPARE)CompareSize; break;
      case uvChanged: SortProc = (PFNLVCOMPARE)CompareModification; break;
      case uvRights: SortProc = (PFNLVCOMPARE)CompareRightsStr; break;
      case uvOwner: SortProc = (PFNLVCOMPARE)CompareOwner; break;
      case uvGroup: SortProc = (PFNLVCOMPARE)CompareGroup; break;
      case uvExt: SortProc = (PFNLVCOMPARE)CompareExtension; break;
      default: assert(false);
    }
    CustomSortItems(SortProc);
  }
#endif
}
//---------------------------------------------------------------------------
bool __fastcall TUnixDirView::GetActive()
{
#ifndef DESIGN_ONLY
  return ((Terminal != NULL) && Terminal->Active);
#else
  return false;
#endif
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::DDDragDetect(int grfKeyState,
  const TPoint &DetectStart, const TPoint &Point, TDragDetectStatus DragStatus)
{
  if ((DragStatus == ddsDrag) && (!Loading) && (MarkedCount > 0))
  {
    TCustomUnixDirView::DDDragDetect(grfKeyState, DetectStart, Point, DragStatus);
  }
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::SetAddParentDir(bool Value)
{
  if (Value != AddParentDir)
  {
    #ifndef DESIGN_ONLY
    if (Terminal) Terminal->Files->IncludeParentDirectory = Value;
    #endif
    TCustomUnixDirView::SetAddParentDir(Value);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TUnixDirView::TargetHasDropHandler(TListItem * /* Item */, int /* Effect */)
{
  return false;
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::DDChooseEffect(int grfKeyState, int &dwEffect)
{
  if ((grfKeyState & (MK_CONTROL | MK_SHIFT)) == 0)
  {
    dwEffect = DROPEFFECT_Copy;
  }

  TCustomDirView::DDChooseEffect(grfKeyState, dwEffect);
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::SetDDAllowMove(bool value)
{
  if (DDAllowMove != value)
  {
    assert(DragDropFilesEx);
    FDDAllowMove = value;
    DragDropFilesEx->SourceEffects = DragSourceEffects;
  }
}
//---------------------------------------------------------------------------
TDropEffectSet __fastcall TUnixDirView::GetDragSourceEffects()
{
  TDropEffectSet Result;
  Result << deCopy;
  if (DDAllowMove) Result << deMove;
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::ChangeDirectory(AnsiString Path)
{
  AnsiString LastFile = "";
  if (ItemFocused) LastFile = ItemFileName(ItemFocused);
  ClearItems();
  DoAnimation(true);
#ifndef DESIGN_ONLY
  try
  {
    FDirLoadedAfterChangeDir = false;
    if (Path == HOMEDIRECTORY)
    {
      Terminal->HomeDirectory();
    }
    else
    // this works even with LockInHome
    if (Path == ROOTDIRECTORY)
    {
      Terminal->CurrentDirectory = ROOTDIRECTORY;
    }
    else
    {
      Terminal->ChangeDirectory(Path);
    }
  }
  __finally
  {
    // changing directory failed, so we load again old directory
    if (!FDirLoadedAfterChangeDir)
    {
      FSelectFile = LastFile;
      Reload(false);
    };
  }
#endif
}
//---------------------------------------------------------------------------
bool __fastcall TUnixDirView::CanEdit(TListItem* Item)
{
#ifndef DESIGN_ONLY
  assert(Terminal);
  return TCustomUnixDirView::CanEdit(Item) && Terminal->IsCapable[fcRename];
#else
  return false;
#endif
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::InternalEdit(const tagLVITEMA & HItem)
{
#ifndef DESIGN_ONLY
  TListItem *Item = GetItemFromHItem(HItem);
  ASSERT_VALID_ITEM;
  FSelectFile = HItem.pszText;
  Terminal->RenameFile(ITEMFILE, HItem.pszText, true);
#endif
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::CreateDirectory(AnsiString DirName)
{
#ifndef DESIGN_ONLY
  assert(Terminal);
  // if file would be created in current directory, select it after reload
  if (UnixExtractFileName(DirName) == DirName)
  {
    FSelectFile = DirName;
  }
  Terminal->CreateDirectory(DirName);
#endif
}
//---------------------------------------------------------------------------
AnsiString __fastcall TUnixDirView::MinimizePath(AnsiString Path, int Length)
{
  return StringReplace(MinimizeName(
    StringReplace(Path, '/', '\\', TReplaceFlags() << rfReplaceAll),
      Canvas, Length), '\\', '/', TReplaceFlags() << rfReplaceAll);
}
//---------------------------------------------------------------------------
bool __fastcall TUnixDirView::GetIsRoot()
{
#ifndef DESIGN_ONLY
  return (PathName == ROOTDIRECTORY);
#else
  return false;
#endif
}
//---------------------------------------------------------------------------
TColor __fastcall TUnixDirView::ItemColor(TListItem * Item)
{
  assert(Item);
#ifndef DESIGN_ONLY
  if (DimmHiddenFiles && !Item->Selected && ITEMFILE->IsHidden)
  {
    return clGrayText;
  }
  else
#endif
  {
    return (TColor)clDefaultItemColor;
  }
}
//---------------------------------------------------------------------------
TDateTime __fastcall TUnixDirView::ItemFileTime(TListItem * Item,
  TDateTimePrecision & Precision)
{
  assert(Item);
#ifndef DESIGN_ONLY
  switch (ITEMFILE->ModificationFmt)
  {
    case mfMDHM:
      Precision = tpMinute;
      break;

    case mfMDY:
      Precision = tpDay;
      break;

    case mfFull:
    default:
      Precision = tpSecond;
      break;
  }
  return ITEMFILE->Modification;
#else
  Precision = tpSecond;
  return Now();
#endif
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::SetShowInaccesibleDirectories(bool value)
{
  if (FShowInaccesibleDirectories != value)
  {
    FShowInaccesibleDirectories = value;
    if (DirOK) Reload(false);
  }
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::AddToDragFileList(TFileList * FileList,
  TListItem * Item)
{
  AnsiString FileName = ItemFullFileName(Item);
  #ifndef DESIGN_ONLY
  if (OnDDDragFileName != NULL)
  {
    OnDDDragFileName(this, ITEMFILE, FileName);
  }
  #endif
  FileList->AddItem(NULL, FileName);
}


