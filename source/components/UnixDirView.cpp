//---------------------------------------------------------------------------
#pragma warn -pch // WORKAROUND (see My.cpp)
#include <vcl.h>
#pragma hdrstop

#include <Common.h>

#include "UnixDirView.h"
#include "UnixDriveView.h"

#include <FileCtrl.hpp>

#ifndef DESIGN_ONLY
#include <CoreMain.h>
#include <Terminal.h>
#include <WinConfiguration.h>
#include <VCLCommon.h>
#endif

#pragma package(smart_init)
#ifndef DESIGN_ONLY
#define ITEMFILE ((TRemoteFile *)(Item->Data))
// noop, previously this tested that the file was in terminal's file listing,
// but that cannot be safely checked now the terminal is used in multithreaded
// environment
#define ASSERT_VALID_ITEM
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
    RegisterComponents(L"Scp", classes, 0);
  }
}
//---------------------------------------------------------------------------
#define HOMEDIRECTORY L""
//---------------------------------------------------------------------------
__fastcall TUnixDirView::TUnixDirView(TComponent* Owner)
        : TCustomUnixDirView(Owner)
{
#ifndef DESIGN_ONLY
  FTerminal = NULL;
#endif
  FCaseSensitive = true;
  FDDAllowMove = false;
  FShowInaccesibleDirectories = true;
  FFullLoad = false;
  FDriveView = NULL;
  FInvalidNameChars = L"/";
  FAnnouncedDriveViewState = NULL;
  DragDropFilesEx->PreferCopy = true;
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
  if (OnContextPopup)
  {
    OnContextPopup(this, ScreenToClient(Where), Handled);
  }
  if (!Handled)
  {
    if (PopupMenu && !PopupMenu->AutoPopup)
    {
      PopupMenu->Popup(Where.x, Where.y);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::DisplayPropertiesMenu()
{
  if (OnDisplayProperties) OnDisplayProperties(this);
}
//---------------------------------------------------------------------------
bool __fastcall TUnixDirView::DoExecFile(TListItem * Item, bool ForceEnter)
{
  bool Result;
#ifndef DESIGN_ONLY
  ASSERT_VALID_ITEM;
  if (ForceEnter)
  {
    PathChanging(true);
    ChangeDirectory(ITEMFILE->FileName);
    Result = false;
  }
  else
#endif
  {
    Result = TCustomDirView::DoExecFile(Item, ForceEnter);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::ExecuteFile(TListItem * Item)
{
#ifndef DESIGN_ONLY
  ASSERT_VALID_ITEM;
  TResolvedDoubleClickAction Action = WinConfiguration->ResolveDoubleClickAction(ITEMFILE->IsDirectory, Terminal);
  if (Action == rdcaChangeDir)
  {
    PathChanging(true);
    ChangeDirectory(ITEMFILE->FileName);
  }
  else
  {
    DebugAssert(Action == rdcaOpen);
    if (ItemFocused != Item) ItemFocused = Item;
    DisplayPropertiesMenu();
  }
#else
  DebugUsedParam(Item);
#endif
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::ExecuteParentDirectory()
{
  PathChanging(true);
#ifndef DESIGN_ONLY
  ChangeDirectory(PARENTDIRECTORY);
#endif
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::ExecuteHomeDirectory()
{
#ifndef DESIGN_ONLY
  // don't select any directory
  PathChanging(false);
  UnicodeString APath = Terminal->SessionData->RemoteDirectory;
  if (WinConfiguration->DefaultDirIsHome && !APath.IsEmpty() &&
      !Terminal->SessionData->UpdateDirectories)
  {
    if (APath[1] != L'/')
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
  FLastPath = L"";
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

  PathChanging(false);
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
  DebugUsedParam(Item);
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
  DebugUsedParam(Item);
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
  DebugUsedParam(Item);
  return false;
#endif
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TUnixDirView::ItemFileName(TListItem * Item)
{
#ifndef DESIGN_ONLY
  ASSERT_VALID_ITEM;
  return ITEMFILE->FileName;
#else
  DebugUsedParam(Item);
  return UnicodeString();
#endif
}
//---------------------------------------------------------------------------
#ifndef DESIGN_ONLY
inline __int64 GetItemFileSize(TRemoteFile * File)
{
  return (File->CalculatedSize >= 0) ? File->CalculatedSize : File->Size;
}
#endif
//---------------------------------------------------------------------------
__int64 __fastcall TUnixDirView::ItemFileSize(TListItem * Item)
{
#ifndef DESIGN_ONLY
  ASSERT_VALID_ITEM;
  return GetItemFileSize(ITEMFILE);
#else
  DebugUsedParam(Item);
  return 0;
#endif
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TUnixDirView::ItemFullFileName(TListItem * Item)
{
#ifndef DESIGN_ONLY
  ASSERT_VALID_ITEM;
  return ITEMFILE->FullFileName;
#else
  DebugUsedParam(Item);
  return UnicodeString();
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
  DebugUsedParam(Item);
  return 0;
#endif
}
//---------------------------------------------------------------------------
TBitmap * __fastcall TUnixDirView::ItemThumbnail(TListItem * Item, const TSize & Size)
{
  TBitmap * Result = NULL;
#ifndef DESIGN_ONLY
  ASSERT_VALID_ITEM;
  if (OnThumbnailNeeded != NULL)
  {
    OnThumbnailNeeded(this, Item, ITEMFILE, Size, Result);
  }
#else
  DebugUsedParam(Item);
  DebugUsedParam(Size);
#endif
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TUnixDirView::ItemMatchesFilter(TListItem * Item,
  const TFileFilter &Filter)
{
#ifndef DESIGN_ONLY
  ASSERT_VALID_ITEM;
  TRemoteFile *File = ITEMFILE;

  return
    ((Filter.Masks.IsEmpty()) ||
     FileNameMatchesMasks(File->FileName, File->IsDirectory, File->Size, File->Modification, Filter.Masks, false) ||
     (File->IsDirectory && Filter.Directories &&
      FileNameMatchesMasks(File->FileName, false, File->Size, File->Modification, Filter.Masks, false)));
#else
  DebugUsedParam(Item);
  DebugUsedParam(Filter);
  return false;
#endif
}
//---------------------------------------------------------------------------
Word __fastcall TUnixDirView::ItemOverlayIndexes(TListItem * Item)
{
#ifndef DESIGN_ONLY
  ASSERT_VALID_ITEM;
  Word Result = TCustomDirView::ItemOverlayIndexes(Item);
  if (ITEMFILE->IsParentDirectory)
  {
    Result |= oiDirUp;
  }
  if (ITEMFILE->IsSymLink)
  {
    Result |= ITEMFILE->BrokenLink ? oiBrokenLink : oiLink;
  }
  if (ITEMFILE->IsEncrypted)
  {
    Result |= oiEncrypted;
  }
  return Result;
#else
  DebugUsedParam(Item);
  return 0;
#endif
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::LoadFiles()
{
#ifndef DESIGN_ONLY
  DebugAssert(Terminal);
  if (DirOK)
  {
    // it's enough if we reach this point, we don't require that loading files into
    // list succeeded. FDirLoadedAfterChangeDir == false tells only that
    // loding file list from server failed, not loading into listview
    FDirLoadedAfterChangeDir = true;

    FFilesSize = 0;
    FHasParentDir = false;
    int VisibleFiles = 0;
    FHiddenCount = 0;
    FFilteredCount = 0;
    DebugAssert(Items->Count == 0); // to make sure that Index matches Items->Count
    for (int Index = 0; Index < Terminal->Files->Count; Index++)
    {
      TRemoteFile *File = Terminal->Files->Files[Index];
      DebugAssert(File);
      if ((!ShowHiddenFiles && File->IsHidden) ||
          (!ShowInaccesibleDirectories && File->IsInaccesibleDirectory))
      {
        FHiddenCount++;
      }
      else if (!FEffectiveMask.IsEmpty() &&
               IsRealFile(File->FileName) &&
               !FileNameMatchesMasks(File->FileName, File->IsDirectory, File->Size, File->Modification, FEffectiveMask, true))
      {
        FFilteredCount++;
      }
      else
      {
        VisibleFiles++;

        FFilesSize += File->Size;
        if (File->IsParentDirectory) FHasParentDir = true;

        TListItem * Item = new TListItem(Items);
        Item->Data = File;
        // Need to add before assigning to .Caption, as its setter call back to owning view.
        // Item assignment is redundant.
        // Index is optimization.
        Item = Items->AddItem(Item, Index);
        // Setting Caption is expensive and it's for display only.
        // Captions of excessive items is delay loaded in GetDisplayInfo.
        if (Index <= 10000)
        {
          Item->Caption = File->FileName;
        }
        if (DebugAlwaysFalse(FFullLoad))
        {
          // this is out of date
          // (missing columns and does not update then file properties are loaded)
          Item->ImageIndex = File->IconIndex;
          Item->SubItems->Add(!File->IsDirectory ? FormatPanelBytes(File->Size, FormatSizeBytes) : UnicodeString());
          Item->SubItems->Add(File->UserModificationStr);
          Item->SubItems->Add(File->RightsStr);
          Item->SubItems->Add(File->Owner.DisplayText);
          Item->SubItems->Add(File->Group.DisplayText);
          Item->SubItems->Add(File->Extension);
        }
      }
    }

    if (DebugAlwaysFalse(OwnerData))
    {
      Items->Count = VisibleFiles;
    }
  }
#endif
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::GetDisplayInfo(TListItem * Item, tagLVITEMW &DispInfo)
{
  if (!FFullLoad)
  {
#ifndef DESIGN_ONLY
    TRemoteFile * File = ITEMFILE;
    // delay loading caption
    if (Item->Caption.IsEmpty())
    {
      Item->Caption = File->FileName;
    }
    if (DispInfo.mask & LVIF_TEXT)
    {
      UnicodeString Value;
      switch (DispInfo.iSubItem) {
        case uvName: Value = File->FileName; break;
        case uvSize:
          {
            __int64 Size;
            if (!File->IsDirectory)
            {
              Size = File->Size;
            }
            else
            {
              Size = File->CalculatedSize;
            }
            if (Size >= 0)
            {
              Value = FormatPanelBytes(Size, FormatSizeBytes);
            }
          }
          break;
        case uvChanged: Value = File->UserModificationStr; break;
        case uvRights: Value = File->RightsStr; break;
        case uvOwner: Value = File->Owner.DisplayText; break;
        case uvGroup: Value = File->Group.DisplayText; break;
        case uvExt: Value = File->Extension; break;
        case uvLinkTarget: Value = File->LinkTo; break;
        case uvType: Value = File->TypeName; break;
        default: DebugFail();
      }
      StrPLCopy(DispInfo.pszText, Value, DispInfo.cchTextMax - 1);
    }

    if (DispInfo.iSubItem == 0 && DispInfo.mask & LVIF_IMAGE)
    {
      DispInfo.iImage = File->IconIndex;
      DispInfo.mask |= LVIF_DI_SETITEM;
    }
#else
  DebugUsedParam(Item);
  DebugUsedParam(DispInfo);
#endif
  }
}
//---------------------------------------------------------------------------
bool __fastcall TUnixDirView::PasteFromClipBoard(UnicodeString TargetPath)
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

    PerformItemDragDropOperation(NULL, DROPEFFECT_COPY, true);
    if (OnDDExecuted != NULL)
    {
      OnDDExecuted(this, DROPEFFECT_COPY);
    }
    Result = true;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::PerformItemDragDropOperation(
  TListItem * Item, int Effect, bool Paste)
{
#ifndef DESIGN_ONLY
  if (OnDDFileOperation)
  {
    // Could be empty if the source application does not provide any files;
    // or if the IDataObject fails GetData, like Visual Studio Code 0.8.0.
    if (DragDropFilesEx->FileList->Count > 0)
    {
      UnicodeString SourceDirectory;
      UnicodeString TargetDirectory;

      SourceDirectory = ExtractFilePath(DragDropFilesEx->FileList->Items[0]->Name);
      if (Item)
      {
        DebugAssert(ITEMFILE->IsDirectory && (Terminal->Files->IndexOf(ITEMFILE) >= 0));
        TargetDirectory = ITEMFILE->FullFileName;
      }
      else
      {
        TargetDirectory = Path;
      }

      bool DoFileOperation = true;
      OnDDFileOperation(
        this, Effect, SourceDirectory, TargetDirectory, Paste, DoFileOperation);
    }
  }
#else
  DebugUsedParam(Item);
  DebugUsedParam(Effect);
  DebugUsedParam(Paste);
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
void __fastcall TUnixDirView::DoSetTerminal(TTerminal * value, bool Replace)
{
  DebugUsedParam(Replace);
  if ((FTerminal != value) ||
      ((FTerminal != NULL) && !FTerminal->Active)) // Abused by TCustomScpExplorerForm::DisconnectSession
  {
    if (FTerminal)
    {
      DebugAssert((FTerminal->OnReadDirectory == DoReadDirectory) || Replace);
      if (FTerminal->OnReadDirectory == DoReadDirectory)
      {
        FTerminal->OnReadDirectory = NULL;
      }
      DebugAssert((FTerminal->OnStartReadDirectory == DoStartReadDirectory) || Replace);
      if (FTerminal->OnStartReadDirectory == DoStartReadDirectory)
      {
        FTerminal->OnStartReadDirectory = NULL;
      }
      if (!value || !value->Files->Loaded)
      {
        ClearItems();
      }
    }
    FTerminal = value;
    PathChanging(false);
    if (FDriveView != NULL)
    {
      FDriveView->Terminal = FTerminal;
    }
    if (FTerminal)
    {
      FTerminal->OnReadDirectory = DoReadDirectory;
      FTerminal->OnStartReadDirectory = DoStartReadDirectory;
      FTerminal->Files->IncludeParentDirectory = AddParentDir;
      if (FTerminal->Files->Loaded)
      {
        DoStartReadDirectory(FTerminal); // just for style and the assertions
        DoReadDirectoryImpl(FTerminal, false);
      }
      else
      {
        PathChanged(); // To clear path combo box
      }
    }
    UpdatePathLabel();
  }
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::SetTerminal(TTerminal * value)
{
  DoSetTerminal(value, false);
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::ReplaceTerminal(TTerminal * value)
{
  DoSetTerminal(value, true);
}
#endif
//---------------------------------------------------------------------------
class TUnixDirViewState : public TObject
{
public:
  std::unique_ptr<TObject> CustomDirViewState;
  std::unique_ptr<TObject> DriveViewState;
};
//---------------------------------------------------------------------------
TObject * __fastcall TUnixDirView::SaveState()
{
  TUnixDirViewState * State = new TUnixDirViewState();
  State->CustomDirViewState.reset(TCustomUnixDirView::SaveState());
  if (FDriveView != NULL)
  {
    State->DriveViewState.reset(FDriveView->SaveState());
  }
  return State;
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::AnnounceState(TObject * State)
{
  TObject * CustomDirViewState = NULL;
  FAnnouncedDriveViewState = NULL;
  if (State != NULL)
  {
    TUnixDirViewState * UnixDirViewState = dynamic_cast<TUnixDirViewState *>(State);
    if (UnixDirViewState != NULL)
    {
      FAnnouncedDriveViewState = UnixDirViewState->DriveViewState.get();
      CustomDirViewState = UnixDirViewState->CustomDirViewState.get();
    }
    else
    {
      // It might be TCustomDirView state from CreateDirViewStateForFocusedItem.
      CustomDirViewState = State;
    }
  }
  TCustomDirView::AnnounceState(CustomDirViewState);
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::RestoreState(TObject * State)
{
  TObject * CustomDirViewState = NULL;
  if (State != NULL)
  {
    TUnixDirViewState * UnixDirViewState = dynamic_cast<TUnixDirViewState *>(State);
    if (UnixDirViewState != NULL)
    {
      CustomDirViewState = UnixDirViewState->CustomDirViewState.get();
    }
    else
    {
      // See the comment in AnnounceState
      CustomDirViewState = State;
    }
  }
  TCustomDirView::RestoreState(CustomDirViewState);
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::DoStartReadDirectory(TObject * /*Sender*/)
{
  DebugAssert(!FLoading);
  FLoading = true;
  if (FOnStartReading != NULL)
  {
    FOnStartReading(this);
  }
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::DoReadDirectory(TObject * Sender, bool ReloadOnly)
{
  DoReadDirectoryImpl(Sender, ReloadOnly);
  if (FOnRead != NULL)
  {
    FOnRead(this);
  }
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::DoReadDirectoryImpl(TObject * /*Sender*/, bool ReloadOnly)
{
  DebugAssert(FLoading);
  FLoading = false;

#ifndef DESIGN_ONLY
  CancelEdit();
  if (Terminal->Active)
  {
    if (ReloadOnly)
    {
      Reload(false);
    }
    else
    {
      Load(true);
    }
    PathChanged();

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
    Load(true);
  }
#else
  DebugUsedParam(ReloadOnly);
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
UnicodeString __fastcall TUnixDirView::GetPathName()
{
#ifndef DESIGN_ONLY
  if (DirOK) return Terminal->CurrentDirectory;
    else
#endif
  return L"";
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TUnixDirView::GetPath()
{
#ifndef DESIGN_ONLY
  if (DirOK) return UnixIncludeTrailingBackslash(Terminal->CurrentDirectory);
    else
#endif
  return L"";
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::SetPath(UnicodeString Value)
{
#ifndef DESIGN_ONLY
  Value = UnixExcludeTrailingBackslash(Value);

  if (Active && (Terminal->CurrentDirectory != Value))
  {
    PathChanging(true);
    Terminal->CurrentDirectory = Value;
  }
#endif
}
//---------------------------------------------------------------------------
#ifndef DESIGN_ONLY
#define COMPARE_NUMBER(Num1, Num2) ( Num1 < Num2 ? -1 : ( Num1 > Num2 ? 1 : 0) )
//---------------------------------------------------------------------------
int __stdcall CompareFile(TListItem * Item1, TListItem * Item2, TUnixDirView * DirView)
{
  DebugAssert((Item1 != NULL) && (Item2 != NULL));
  TRemoteFile * File1 = DebugNotNull((TRemoteFile *)(Item1->Data));
  TRemoteFile * File2 = DebugNotNull((TRemoteFile *)(Item2->Data));

  int Result;
  if (File1->IsParentDirectory && !File2->IsParentDirectory)
  {
    Result = -1;
  }
  else if (!File1->IsParentDirectory && File2->IsParentDirectory)
  {
    Result = 1;
  }
  else if (File1->IsDirectory && !File2->IsDirectory)
  {
    Result = -1;
  }
  else if (!File1->IsDirectory && File2->IsDirectory)
  {
    Result = 1;
  }
  else
  {
    Result = 0;

    if (File1->IsDirectory && DirView->AlwaysSortDirectoriesByName)
    {
      // fallback
    }
    else
    {
      switch (DirView->SortColumn)
      {
        case uvName:
          // fallback
          break;

        case uvSize:
          Result = COMPARE_NUMBER(GetItemFileSize(File1), GetItemFileSize(File2));
          break;

        case uvChanged:
          Result = COMPARE_NUMBER(File1->Modification, File2->Modification);
          break;

        case uvRights:
          Result = AnsiCompareText(File1->RightsStr, File2->RightsStr);
          break;

        case uvOwner:
          Result = File1->Owner.Compare(File2->Owner);
          break;

        case uvGroup:
          Result = File1->Group.Compare(File2->Group);
          break;

        case uvExt:
          // Duplicated in uvType branch
          if (!File1->IsDirectory)
          {
            Result = CompareLogicalText(File1->Extension, File2->Extension, DirView->NaturalOrderNumericalSorting);
          }
          else
          {
            // fallback
          }
          break;

        case uvLinkTarget:
          Result = CompareLogicalText(File1->LinkTo, File2->LinkTo, DirView->NaturalOrderNumericalSorting);
          break;

        case uvType:
          Result = CompareLogicalText(File1->TypeName, File2->TypeName, DirView->NaturalOrderNumericalSorting);
          // fallback to uvExt
          if ((Result == 0) && !File1->IsDirectory)
          {
            Result = CompareLogicalText(File1->Extension, File2->Extension, DirView->NaturalOrderNumericalSorting);
          }
          break;

        default:
          DebugFail();
      }
    }

    if (Result == 0)
    {
      Result = CompareLogicalText(File1->FileName, File2->FileName, DirView->NaturalOrderNumericalSorting);
    }

    if (!DirView->UnixColProperties->SortAscending)
    {
      Result = -Result;
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
#undef COMPARE_NUMBER
#endif
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::SortItems()
{
#ifndef DESIGN_ONLY
  if (HandleAllocated())
  {
    CustomSortItems(CompareFile);
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
void __fastcall TUnixDirView::DDChooseEffect(int grfKeyState, int &dwEffect, int PreferredEffect)
{
  if ((grfKeyState & (MK_CONTROL | MK_SHIFT)) == 0)
  {
    dwEffect = DROPEFFECT_COPY;
  }

  TCustomDirView::DDChooseEffect(grfKeyState, dwEffect, PreferredEffect);
}
//---------------------------------------------------------------------------
TDropEffectSet __fastcall TUnixDirView::GetDragSourceEffects()
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
void __fastcall TUnixDirView::ChangeDirectory(UnicodeString Path)
{
  UnicodeString LastFile = L"";
  if (ItemFocused) LastFile = ItemFileName(ItemFocused);
  ClearItems();
#ifndef DESIGN_ONLY
  try
  {
    FDirLoadedAfterChangeDir = false;
    if (Path == HOMEDIRECTORY)
    {
      Terminal->HomeDirectory();
    }
    else if (Path == ROOTDIRECTORY)
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
  DebugAssert(Terminal);
  return TCustomUnixDirView::CanEdit(Item) && Terminal->IsCapable[fcRename];
#else
  DebugUsedParam(Item);
  return false;
#endif
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::InternalEdit(const tagLVITEMW & HItem)
{
#ifndef DESIGN_ONLY
  TListItem *Item = GetItemFromHItem(HItem);
  ASSERT_VALID_ITEM;
  LoadEnabled = true;
  if (ITEMFILE->FileName != HItem.pszText)
  {
    FSelectFile = HItem.pszText;
    Terminal->RenameFile(ITEMFILE, HItem.pszText);
  }
#else
  DebugUsedParam(HItem);
#endif
}
//---------------------------------------------------------------------------
int __fastcall TUnixDirView::HiddenCount()
{
  return FHiddenCount;
}
//---------------------------------------------------------------------------
int __fastcall TUnixDirView::FilteredCount()
{
  return FFilteredCount;
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::CreateDirectory(UnicodeString DirName)
{
  CreateDirectoryEx(DirName, NULL);
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::CreateDirectoryEx(UnicodeString DirName, const TRemoteProperties * Properties)
{
#ifndef DESIGN_ONLY
  DebugAssert(Terminal);
  // if file would be created in current directory, select it after reload
  if (UnixExtractFileName(DirName) == DirName)
  {
    FSelectFile = DirName;
  }
  Terminal->CreateDirectory(DirName, Properties);
#else
  DebugUsedParam(Properties);
#endif
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
  DebugAssert(Item);
#ifndef DESIGN_ONLY
  if (DimmHiddenFiles && !Item->Selected && ITEMFILE->IsHidden)
  {
    return clGrayText;
  }
  else
#else
  DebugUsedParam(Item);
#endif
  {
    return (TColor)clDefaultItemColor;
  }
}
//---------------------------------------------------------------------------
TDateTime __fastcall TUnixDirView::ItemFileTime(TListItem * Item,
  TDateTimePrecision & Precision)
{
  DebugAssert(Item);
#ifndef DESIGN_ONLY
  switch (ITEMFILE->ModificationFmt)
  {
    case mfNone:
      Precision = tpNone;
      break;

    case mfMDHM:
    case mfYMDHM:
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
  DebugUsedParam(Item);
  Precision = tpSecond;
  return Now();
#endif
}
//---------------------------------------------------------------------------
TObject * __fastcall TUnixDirView::ItemData(TListItem * Item)
{
#ifndef DESIGN_ONLY
  return ITEMFILE;
#else
  DebugUsedParam(Item);
  return NULL;
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
  UnicodeString FileName = ItemFullFileName(Item);
  #ifndef DESIGN_ONLY
  if (OnDDDragFileName != NULL)
  {
    OnDDDragFileName(this, ITEMFILE, FileName);
  }
  #endif
  FileList->AddItem(NULL, FileName);
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::UpdatePathLabelCaption()
{
  if (Terminal != NULL)
  {
    TCustomDirView::UpdatePathLabelCaption();
  }
  else
  {
    PathLabel->Caption = UnicodeString();
    PathLabel->Mask = UnicodeString();
  }
}
//---------------------------------------------------------------------------
void __fastcall TUnixDirView::SetItemCalculatedSize(TListItem * Item, __int64 Size)
{
  __int64 OldSize;
  #ifndef DESIGN_ONLY
  OldSize = ITEMFILE->CalculatedSize;
  ITEMFILE->CalculatedSize = Size;
  #else
  OldSize = -1;
  #endif
  ItemCalculatedSizeUpdated(Item, OldSize, Size);
}
