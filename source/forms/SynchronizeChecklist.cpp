//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>

#include "WinInterface.h"
#include "SynchronizeChecklist.h"

#include <Terminal.h>
#include <TextsWin.h>
#include <CoreMain.h>

#include <VCLCommon.h>
#include <Tools.h>
#include <BaseUtils.hpp>
#include <Math.hpp>
#include <WinConfiguration.h>
#include <GUITools.h>
#include <TerminalManager.h>
#include <System.IOUtils.hpp>
#include <System.StrUtils.hpp>
#include <algorithm>
//---------------------------------------------------------------------
#pragma link "IEListView"
#pragma link "NortonLikeListView"
#pragma link "PngImageList"
#pragma resource "*.dfm"
//---------------------------------------------------------------------
const int ImageColumnIndex = 4;
//---------------------------------------------------------------------
bool __fastcall DoSynchronizeChecklistDialog(TSynchronizeChecklist * Checklist,
  TSynchronizeMode Mode, int Params,
  const UnicodeString LocalDirectory, const UnicodeString RemoteDirectory,
  TCustomCommandMenuEvent OnCustomCommandMenu, TFullSynchronizeEvent OnSynchronize,
  TQueueSynchronizeEvent OnQueueSynchronize,
  TSynchronizeChecklistCalculateSize OnSynchronizeChecklistCalculateSize, TSynchronizeMoveEvent OnSynchronizeMove,
  TSynchronizeBrowseEvent OnSynchronizeBrowse, void * Token)
{
  std::unique_ptr<TSynchronizeChecklistDialog> Dialog(
    new TSynchronizeChecklistDialog(
      Application, Mode, Params, LocalDirectory, RemoteDirectory, OnCustomCommandMenu, OnSynchronize,
      OnQueueSynchronize, OnSynchronizeChecklistCalculateSize, OnSynchronizeMove, OnSynchronizeBrowse, Token));
  return Dialog->Execute(Checklist);
}
//---------------------------------------------------------------------
__fastcall TSynchronizeChecklistDialog::TSynchronizeChecklistDialog(
  TComponent * AOwner, TSynchronizeMode Mode, int Params,
  const UnicodeString & LocalDirectory, const UnicodeString & RemoteDirectory,
  TCustomCommandMenuEvent OnCustomCommandMenu, TFullSynchronizeEvent OnSynchronize,
  TQueueSynchronizeEvent OnQueueSynchronize,
  TSynchronizeChecklistCalculateSize OnSynchronizeChecklistCalculateSize, TSynchronizeMoveEvent OnSynchronizeMove,
  TSynchronizeBrowseEvent OnSynchronizeBrowse, void * Token)
  : TForm(AOwner)
{
  FFormRestored = false;
  FMode = Mode;
  FParams = Params;
  FLocalDirectory = ExcludeTrailingBackslash(LocalDirectory);
  FRemoteDirectory = UnixExcludeTrailingBackslash(RemoteDirectory);
  FOnCustomCommandMenu = OnCustomCommandMenu;
  FOnSynchronizeChecklistCalculateSize = OnSynchronizeChecklistCalculateSize;
  FOnSynchronizeMove = OnSynchronizeMove;
  FOnSynchronizeBrowse = OnSynchronizeBrowse;
  DebugAssert(OnSynchronize != NULL);
  FOnSynchronize = OnSynchronize;
  FOnQueueSynchronize = OnQueueSynchronize;
  FToken = Token;
  UseSystemSettings(this);
  UseDesktopFont(ListView);
  UseDesktopFont(StatusBar);
  FChecklist = NULL;
  FChangingItem = NULL;
  FChangingItemIgnore = false;
  FChangingItemMass = false;
  FSynchronizing = false;
  FMoveCandidatesValidForSort = MaxInt;
  FDirectories = 0;

  SelectScaledImageList(ActionImages);

  FOrigListViewWindowProc = ListView->WindowProc;
  ListView->WindowProc = ListViewWindowProc;
  FOrigStatusBarWindowProc = StatusBar->WindowProc;
  StatusBar->WindowProc = StatusBarWindowProc;

  UpdateImages();

  CustomCommandsAction->Visible = (FOnCustomCommandMenu != NULL);
  // button visibility cannot be bound to action visibility
  CustomCommandsButton2->Visible = CustomCommandsAction->Visible;
  MenuButton(CustomCommandsButton2);
  MenuButton(ToolsMenuButton);

  // Other dialogs use !IsMainFormLike
  if (FOnQueueSynchronize != NULL)
  {
    OkButton->Style = TCustomButton::bsSplitButton;
  }
}
//---------------------------------------------------------------------
__fastcall TSynchronizeChecklistDialog::~TSynchronizeChecklistDialog()
{
  StatusBar->WindowProc = FOrigStatusBarWindowProc;
  ListView->WindowProc = FOrigListViewWindowProc;
}
//---------------------------------------------------------------------
bool __fastcall TSynchronizeChecklistDialog::Execute(TSynchronizeChecklist * Checklist)
{
  FChecklist = Checklist;

  bool Result = (ShowModal() == DefaultResult(this));

  if (Result)
  {
    TSynchronizeChecklistConfiguration FormConfiguration =
      CustomWinConfiguration->SynchronizeChecklist;
    FormConfiguration.ListParams = ListView->ColProperties->ParamsStr;

    UnicodeString WindowParams = FormConfiguration.WindowParams;
    // if there is no main window, keep previous "custom pos" indication,
    bool CustomPos = (StrToIntDef(CutToChar(WindowParams, L';', true), 0) != 0);
    if (!IsMainFormLike(this))
    {
      CustomPos = (Application->MainForm->BoundsRect != BoundsRect);
    }
    FormConfiguration.WindowParams =
      FORMAT(L"%d;%s", ((CustomPos ? 1 : 0), StoreForm(this)));

    CustomWinConfiguration->SynchronizeChecklist = FormConfiguration;
  }

  if (FException.get() != NULL)
  {
    RethrowException(FException.get());
  }

  return Result;
}
//---------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::UpdateCaption()
{
  TTerminalManager * Manager = TTerminalManager::Instance();
  UnicodeString Title = Manager->GetAppProgressTitle();
  UnicodeString StatusTitle = LoadStr(FSynchronizing ? SYNCHRONIZE_PROGRESS_SYNCHRONIZE2 : SYNCHRONIZE_CHECKLIST_CAPTION);
  if (Title.Pos(StatusTitle) <= 0)
  {
    AddToList(Title, StatusTitle, TitleSeparator);
  }
  Caption = Manager->FormatFormCaptionWithSession(this, Title);
}
//---------------------------------------------------------------------------
static bool IsTransferNewAction(TSynchronizeChecklist::TAction Action)
{
  return (Action == TSynchronizeChecklist::saUploadNew) || (Action == TSynchronizeChecklist::saDownloadNew);
}
//---------------------------------------------------------------------------
static TSynchronizeChecklist::TAction GetOppositeMoveAction(TSynchronizeChecklist::TAction Action1)
{
  switch (Action1)
  {
    case TSynchronizeChecklist::saUploadNew:
      return TSynchronizeChecklist::saDeleteRemote;
    case TSynchronizeChecklist::saDownloadNew:
      return TSynchronizeChecklist::saDeleteLocal;
    case TSynchronizeChecklist::saDeleteRemote:
      return TSynchronizeChecklist::saUploadNew;
    case TSynchronizeChecklist::saDeleteLocal:
      return TSynchronizeChecklist::saDownloadNew;
    default:
      return TSynchronizeChecklist::saNone;
  }
}
//---------------------------------------------------------------------
struct TMoveActionData
{
  TSynchronizeChecklist::TAction Action;
  bool AllItemsFiles;
  bool AllItemsDirectories;
  int ItemsCount;
  std::unique_ptr<TStrings> FileList;
  typedef std::vector<const TSynchronizeChecklist::TItem *> TChecklistItems;
  TChecklistItems ChecklistItems;

  TMoveActionData()
  {
    Action = TSynchronizeChecklist::saNone;
    AllItemsFiles = true;
    AllItemsDirectories = true;
    ItemsCount = 0;
  }

  bool Collect(const TSynchronizeChecklist::TItem * ChecklistItem, TSynchronizeChecklist::TAction AAction, bool CollectFileList)
  {
    bool Result = (ItemsCount == 0) || (Action == AAction);
    if (Result)
    {
      Action = AAction;
      if (ChecklistItem->IsDirectory)
      {
        AllItemsFiles = false;
      }
      else
      {
        AllItemsDirectories = false;
      }
      ItemsCount++;

      if (CollectFileList)
      {
        UnicodeString FileName;
        TObject * Object;
        if (Action == TSynchronizeChecklist::saDeleteRemote)
        {
          FileName = ChecklistItem->GetRemotePath();
          Object = ChecklistItem->RemoteFile;
        }
        else if (Action == TSynchronizeChecklist::saDeleteLocal)
        {
          FileName = ChecklistItem->GetLocalPath();
        }

        bool CollectFile = !FileName.IsEmpty();
        DebugAssert(CollectFile == IsTransferNewAction(GetOppositeMoveAction(Action)));
        if (CollectFile)
        {
          if (FileList.get() == NULL)
          {
            FileList.reset(new TStringList());
          }
          FileList->AddObject(FileName, Object);
        }

        ChecklistItems.push_back(ChecklistItem);
      }
    }
    return Result;
  }

  bool IsOnlyDirectoryTransferAction()
  {
    return
      IsTransferNewAction(Action) &&
      (ItemsCount == 1) &&
      AllItemsDirectories &&
      DebugAlwaysTrue(!AllItemsFiles);
  }
};
//---------------------------------------------------------------------
struct TMoveData
{
  TMoveActionData FirstAction;
  TMoveActionData SecondAction;
  bool ThreeActions;

  TMoveData(bool CollectFileList)
  {
    ThreeActions = false;
    FCollectFileList = CollectFileList;
  }

  void Collect(const TSynchronizeChecklist::TItem * ChecklistItem, TSynchronizeChecklist::TAction Action)
  {
    if (!FirstAction.Collect(ChecklistItem, Action, FCollectFileList) &&
        !SecondAction.Collect(ChecklistItem, Action, FCollectFileList))
    {
      ThreeActions = true;
    }
  }

private:
  bool FCollectFileList;
};
//---------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::UpdateControls()
{
  UpdateCaption();

  StatusBar->Invalidate();

  bool AllChecked = true;
  bool AllUnchecked = true;
  bool AnyBoth = false;
  bool AnyNonBoth = false;
  bool AnyDirectory = false;
  TMoveData MoveData(false);
  TListItem * Item = NULL;
  while (IterateSelectedItems(Item))
  {
    const TSynchronizeChecklist::TItem * ChecklistItem = GetChecklistItem(Item);
    TSynchronizeChecklist::TAction Action = GetChecklistItemAction(ChecklistItem);

    if ((Action == TSynchronizeChecklist::saUploadUpdate) ||
        (Action == TSynchronizeChecklist::saDownloadUpdate))
    {
      AnyBoth = true;
    }
    else
    {
      AnyNonBoth = true;
    }

    MoveData.Collect(ChecklistItem, Action);

    if (Item->Checked)
    {
      AllUnchecked = false;
    }
    else
    {
      AllChecked = false;
    }
    if (ChecklistItem->IsDirectory)
    {
      AnyDirectory = true;
    }
  }

  int SelCount = ListView->SelCount;
  EnableControl(OkButton, (FChecked[0] > 0) && !FSynchronizing);
  EnableControl(CancelButton, !FSynchronizing);
  EnableControl(HelpButton, !FSynchronizing);
  CheckAction->Enabled = !AllChecked && !FSynchronizing;
  UncheckAction->Enabled = !AllUnchecked && !FSynchronizing;
  CheckAllAction->Enabled = (FChecked[0] < FTotals[0]) && !FSynchronizing;
  UncheckAllAction->Enabled = (FChecked[0] > 0) && !FSynchronizing;
  CheckDirectoryAction->Enabled = CheckAllAction->Enabled; // sic
  UncheckDirectoryAction->Enabled = UncheckAllAction->Enabled; // sic
  CustomCommandsAction->Enabled = AnyBoth && !AnyNonBoth && DebugAlwaysTrue(!FSynchronizing);
  ReverseAction->Enabled = (SelCount > 0) && DebugAlwaysTrue(!FSynchronizing);
  MoveAction->Enabled =
    // All actions are of exactly two and opposite types
    (MoveData.SecondAction.Action != TSynchronizeChecklist::saNone) &&
    !MoveData.ThreeActions &&
    (GetOppositeMoveAction(MoveData.FirstAction.Action) == MoveData.SecondAction.Action) &&
    // ... and there are exactly two of them of the same file/dir type
    (((SelCount == 2) && (MoveData.FirstAction.AllItemsFiles == MoveData.SecondAction.AllItemsFiles)) ||
    // ... or the "transfer new" side is exactly one directory
     MoveData.FirstAction.IsOnlyDirectoryTransferAction() ||
     MoveData.SecondAction.IsOnlyDirectoryTransferAction());
  CalculateSizeAction->Enabled = (SelCount > 0) && AnyDirectory && DebugAlwaysTrue(!FSynchronizing);
  CalculateSizeAllAction->Enabled = (FDirectories > 0) && !FSynchronizing;
  TSynchronizeChecklist::TAction SelectedItemAction =
    (SelCount == 1) ? GetChecklistItemAction(GetChecklistItem(ListView->Selected)) : TSynchronizeChecklist::saNone;
  BrowseLocalAction->Enabled = (SelCount == 1) && (SelectedItemAction != TSynchronizeChecklist::saDeleteRemote);
  BrowseRemoteAction->Enabled = (SelCount == 1) && (SelectedItemAction != TSynchronizeChecklist::saDeleteLocal);

  int Count = ListView->Items->Count;
  DebugAssert(FTotals[0] == Count);
  SelectAllAction->Enabled = (SelCount < Count) && !FSynchronizing;
  FindMoveCandidateAction->Enabled = (Count > 0) && !FSynchronizing;
}
//---------------------------------------------------------------------------
bool __fastcall TSynchronizeChecklistDialog::GetWindowParams(UnicodeString & WindowParams)
{
  WindowParams = CustomWinConfiguration->SynchronizeChecklist.WindowParams;
  bool CustomPos = (StrToIntDef(CutToChar(WindowParams, L';', true), 0) != 0);
  return CustomPos || IsMainFormLike(this);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::CreateParams(TCreateParams & Params)
{
  UnicodeString WindowParams;
  if (GetWindowParams(WindowParams))
  {
    // This is only to set correct TForm::Position. Actual bounds are set later after DPI scaling
    RestoreForm(WindowParams, this, true);
  }
  TForm::CreateParams(Params);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::AddSubItem(TListItem * Item, int & Index, const UnicodeString & S)
{
  if (Index < Item->SubItems->Count)
  {
    Item->SubItems->Strings[Index] = S;
  }
  else
  {
    DebugAssert(Index == Item->SubItems->Count);
    Item->SubItems->Add(S);
  }
  Index++;
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::LoadItem(TListItem * Item)
{
  UnicodeString S;
  const TSynchronizeChecklist::TItem * ChecklistItem = GetChecklistItem(Item);
  if (ChecklistItem->ImageIndex >= 0)
  {
    Item->ImageIndex = ChecklistItem->ImageIndex;
  }
  else
  {
    Item->ImageIndex = FakeFileImageIndex(ChecklistItem->GetFileName(),
      FLAGMASK(ChecklistItem->IsDirectory, FILE_ATTRIBUTE_DIRECTORY));
  }

  S = ChecklistItem->GetFileName();
  if (ChecklistItem->IsDirectory)
  {
    S = IncludeTrailingBackslash(S);
  }
  Item->Caption = S;

  int Index = 0;

  TSynchronizeChecklist::TAction Action = GetChecklistItemAction(ChecklistItem);

  if (Action == TSynchronizeChecklist::saDeleteRemote)
  {
    AddSubItem(Item, Index, L"");
    AddSubItem(Item, Index, L"");
    AddSubItem(Item, Index, L"");
  }
  else
  {
    S = ChecklistItem->Local.Directory;
    if (AnsiSameText(FLocalDirectory, S.SubString(1, FLocalDirectory.Length())))
    {
      S[1] = L'.';
      S.Delete(2, FLocalDirectory.Length() - 1);
    }
    else
    {
      DebugFail();
    }
    AddSubItem(Item, Index, S);
    if (Action == TSynchronizeChecklist::saDownloadNew)
    {
      AddSubItem(Item, Index, L"");
      AddSubItem(Item, Index, L"");
    }
    else
    {
      if (!ChecklistItem->HasSize())
      {
        AddSubItem(Item, Index, L"");
      }
      else
      {
        AddSubItem(Item, Index,
          FormatPanelBytes(ChecklistItem->Local.Size, WinConfiguration->FormatSizeBytes));
      }
      AddSubItem(Item, Index,
        UserModificationStr(ChecklistItem->Local.Modification,
          ChecklistItem->Local.ModificationFmt));
    }
  }

  AddSubItem(Item, Index, L"");
  DebugAssert(Index == ImageColumnIndex);

  if (Action == TSynchronizeChecklist::saDeleteLocal)
  {
    AddSubItem(Item, Index, L"");
    AddSubItem(Item, Index, L"");
    AddSubItem(Item, Index, L"");
  }
  else
  {
    S = ChecklistItem->Remote.Directory;
    if (AnsiSameText(FRemoteDirectory, S.SubString(1, FRemoteDirectory.Length())))
    {
      S[1] = L'.';
      S.Delete(2, FRemoteDirectory.Length() - 1);
    }
    else
    {
      DebugFail();
    }
    AddSubItem(Item, Index, S);
    if (Action == TSynchronizeChecklist::saUploadNew)
    {
      AddSubItem(Item, Index, L"");
      AddSubItem(Item, Index, L"");
    }
    else
    {
      if (!ChecklistItem->HasSize())
      {
        AddSubItem(Item, Index, L"");
      }
      else
      {
        AddSubItem(Item, Index,
          FormatPanelBytes(ChecklistItem->Remote.Size, WinConfiguration->FormatSizeBytes));
      }
      AddSubItem(Item, Index, UserModificationStr(ChecklistItem->Remote.Modification,
        ChecklistItem->Remote.ModificationFmt));
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::CountItemSize(const TSynchronizeChecklist::TItem * ChecklistItem, int Factor)
{
  TSynchronizeChecklist::TAction Action = GetChecklistItemAction(ChecklistItem);
  int ActionIndex = int(Action);
  __int64 ItemSize = ChecklistItem->GetSize(Action);
  FCheckedSize[ActionIndex] += Factor * ItemSize;
  FCheckedSize[0] += Factor * ItemSize;
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::CountItem(const TSynchronizeChecklist::TItem * ChecklistItem, int Factor)
{
  int ActionIndex = int(GetChecklistItemAction(ChecklistItem));
  FChecked[ActionIndex] += Factor;
  FChecked[0] += Factor;

  CountItemSize(ChecklistItem, Factor);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::CountItemTotal(const TSynchronizeChecklist::TItem * ChecklistItem, int Factor)
{
  FTotals[0] += Factor;
  int ActionIndex = int(GetChecklistItemAction(ChecklistItem));
  FTotals[ActionIndex] += Factor;
  if (ChecklistItem->IsDirectory)
  {
    FDirectories += Factor;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::LoadList()
{
  memset(&FTotals, 0, sizeof(FTotals));
  memset(&FChecked, 0, sizeof(FChecked));
  memset(&FCheckedSize, 0, sizeof(FCheckedSize));
  FDirectories = 0;

  ListView->Items->BeginUpdate();
  try
  {
    ListView->Items->Clear();
    for (int Index = 0; Index < FChecklist->Count; Index++)
    {
      const TSynchronizeChecklist::TItem * ChecklistItem =
        FChecklist->Item[ListView->Items->Count];
      FChangingItemIgnore = true;
      try
      {
        TListItem * Item = ListView->Items->Add();
        TSynchronizeChecklist::TAction Action = ChecklistItem->Action;
        FActions.insert(std::make_pair(ChecklistItem, Action));
        FChecklistToListViewMap.insert(std::make_pair(ChecklistItem, Item));
        Item->Data = const_cast<TSynchronizeChecklist::TItem *>(ChecklistItem);
        Item->Checked = ChecklistItem->Checked;
        LoadItem(Item);
      }
      __finally
      {
        FChangingItemIgnore = false;
      }

      CountItemTotal(ChecklistItem, 1);
      if (ChecklistItem->Checked)
      {
        CountItem(ChecklistItem, 1);
      }
    }
  }
  __finally
  {
    ListView->Items->EndUpdate();
  }

  ListView->AlphaSort();
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::FormShow(TObject * /*Sender*/)
{
  // Moved here from CreateParams (see also TEditorForm::CreateParams), because there it breaks per-monitor DPI.
  // For example BoundsRect is matched to the main form too soon, so it gets rescaled later.
  // Also it happens before constructor, what causes UseDesktopFont-flagged controls to rescale twice.
  // But Position is already set in the CreateParams, as it cannot be set here anymore.
  if (!FFormRestored)
  {
    FFormRestored = True;
    UnicodeString WindowParams;
    if (GetWindowParams(WindowParams))
    {
      RestoreForm(WindowParams, this);
    }
    else
    {
      BoundsRect = Application->MainForm->BoundsRect;
    }
    FlashOnBackground();
  }

  ListView->ColProperties->ParamsStr = CustomWinConfiguration->SynchronizeChecklist.ListParams;

  LoadList();
  UpdateStatusBarSize();
}
//---------------------------------------------------------------------------
TRect __fastcall TSynchronizeChecklistDialog::GetColumnHeaderRect(int Index)
{
  HWND HeaderHandle = ListView_GetHeader(ListView->Handle);
  TRect R;
  Header_GetItemRect(HeaderHandle, Index, &R);

  // Can be simplified using GetScrollPos
  TScrollInfo ScrollInfo;
  ZeroMemory(&ScrollInfo, sizeof(ScrollInfo));
  ScrollInfo.cbSize = sizeof(ScrollInfo);
  ScrollInfo.fMask = SIF_POS;
  GetScrollInfo(ListView->Handle, SB_HORZ, &ScrollInfo);

  R.Left -= ScrollInfo.nPos;
  R.Right -= ScrollInfo.nPos;

  return R;
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::ListViewWindowProc(TMessage & Message)
{
  if (Message.Msg == CN_NOTIFY)
  {
    TWMNotify & NotifyMessage = reinterpret_cast<TWMNotify &>(Message);
    if (NotifyMessage.NMHdr->code == NM_CUSTOMDRAW)
    {
      // workaround
      // Due to a bug in VCL, OnAdvancedCustomDrawSubItem is not called for any
      // other stage except for cdPrePaint. So we must call it ourselves.
      TNMLVCustomDraw * CustomDraw =
        reinterpret_cast<TNMLVCustomDraw *>(NotifyMessage.NMHdr);
      if (FLAGSET(CustomDraw->nmcd.dwDrawStage, CDDS_ITEM) &&
          FLAGSET(CustomDraw->nmcd.dwDrawStage, CDDS_SUBITEM) &&
          FLAGSET(CustomDraw->nmcd.dwDrawStage, CDDS_ITEMPOSTPAINT) &&
          (CustomDraw->iSubItem == ImageColumnIndex) &&
          (ActionImages->Width <= ListView->Columns->Items[CustomDraw->iSubItem]->Width))
      {
        TListItem * Item = ListView->Items->Item[CustomDraw->nmcd.dwItemSpec];
        const TSynchronizeChecklist::TItem * ChecklistItem = GetChecklistItem(Item);

        TRect HeaderR = GetColumnHeaderRect(CustomDraw->iSubItem);
        TRect R = Item->DisplayRect(drBounds);
        R.Left = HeaderR.Left + (HeaderR.Width() - ActionImages->Width) / 2;
        R.Right = HeaderR.Right;

        // workaround
        // doing this from ListViewAdvancedCustomDraw corrupts list view on Windows 7
        ImageList_Draw(reinterpret_cast<HIMAGELIST>(ActionImages->Handle),
          int(GetChecklistItemAction(ChecklistItem)), CustomDraw->nmcd.hdc,
          R.Left, ((R.Top + R.Bottom - ActionImages->Height) / 2), ILD_TRANSPARENT);
      }
    }

    FOrigListViewWindowProc(Message);
  }
  else if (Message.Msg == CM_HINTSHOW)
  {
    ListViewHintShow(reinterpret_cast<TCMHintShow &>(Message));
  }
  else if (Message.Msg == WM_WANTS_SCREEN_TIPS)
  {
    Message.Result = 1;
  }
  else
  {
    FOrigListViewWindowProc(Message);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::ListViewHintShow(TCMHintShow & HintShow)
{
  TLVHitTestInfo HitTest;
  HitTest.pt = HintShow.HintInfo->CursorPos;
  int Index = ListView_SubItemHitTest(ListView->Handle, &HitTest);
  if ((Index >= 0) && (HitTest.iSubItem == ImageColumnIndex))
  {
    TListItem * Item = ListView->Items->Item[Index];
    const TSynchronizeChecklist::TItem * ChecklistItem = GetChecklistItem(Item);
    int ActionHint = 0;
    switch (int(GetChecklistItemAction(ChecklistItem)))
    {
      case TSynchronizeChecklist::saUploadNew:
        ActionHint = SYNCHRONIZE_CHECKLIST_UPLOAD_NEW;
        break;
      case TSynchronizeChecklist::saDownloadNew:
        ActionHint = SYNCHRONIZE_CHECKLIST_DOWNLOAD_NEW;
        break;
      case TSynchronizeChecklist::saUploadUpdate:
        ActionHint = SYNCHRONIZE_CHECKLIST_UPLOAD_UPDATE;
        break;
      case TSynchronizeChecklist::saDownloadUpdate:
        ActionHint = SYNCHRONIZE_CHECKLIST_DOWNLOAD_UPDATE;
        break;
      case TSynchronizeChecklist::saDeleteRemote:
        ActionHint = SYNCHRONIZE_CHECKLIST_DELETE_REMOTE;
        break;
      case TSynchronizeChecklist::saDeleteLocal:
        ActionHint = SYNCHRONIZE_CHECKLIST_DELETE_LOCAL;
        break;
    }
    if (DebugAlwaysTrue(ActionHint != 0))
    {
      HintShow.HintInfo->HintStr = FORMAT(L"%s|%s", (LoadStr(ActionHint), LoadStr(SYNCHRONIZE_CHECKLIST_REVERSE)));
      ListView_GetSubItemRect(ListView->Handle, Index, ImageColumnIndex, LVIR_BOUNDS, &HintShow.HintInfo->CursorRect);
      HintShow.Result = 0;
    }
  }

  FOrigListViewWindowProc(reinterpret_cast<TMessage &>(HintShow));
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::StatusBarHintShow(TCMHintShow & HintShow)
{
  int IPanel = PanelAt(HintShow.HintInfo->CursorPos.x);

  if (IPanel >= 0)
  {
    TStatusPanel * Panel = StatusBar->Panels->Items[IPanel];
    HintShow.HintInfo->HintStr = FORMAT(L"%s|%s", (Panel->Text, StatusBar->Hint));

    HintShow.HintInfo->CursorRect.Left = 0;
    while (IPanel > 0)
    {
      IPanel--;
      HintShow.HintInfo->CursorRect.Left += StatusBar->Panels->Items[IPanel]->Width;
    }

    HintShow.HintInfo->CursorRect.Top = 0;
    HintShow.HintInfo->CursorRect.Bottom = StatusBar->ClientHeight;
    HintShow.HintInfo->CursorRect.Right = HintShow.HintInfo->CursorRect.Left + Panel->Width;

    HintShow.Result = 0;
  }

  FOrigListViewWindowProc(reinterpret_cast<TMessage &>(HintShow));
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::StatusBarWindowProc(TMessage & Message)
{
  if (Message.Msg == WM_WANTS_SCREEN_TIPS)
  {
    Message.Result = 1;
  }
  else if (Message.Msg == CM_HINTSHOW)
  {
    StatusBarHintShow(reinterpret_cast<TCMHintShow &>(Message));
  }
  else
  {
    FOrigStatusBarWindowProc(Message);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::ListViewAdvancedCustomDrawSubItem(
  TCustomListView * /*Sender*/, TListItem * /*Item*/, int /*SubItem*/,
  TCustomDrawState /*State*/, TCustomDrawStage /*Stage*/, bool & /*DefaultDraw*/)
{
  // this is just fake handler that makes the list view request custom draw notification above
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::StatusBarDrawPanel(
  TStatusBar * StatusBar, TStatusPanel * Panel, const TRect & Rect)
{
  bool Possible;
  int ActionIndex = Panel->Index;
  TSynchronizeChecklist::TAction Action = TSynchronizeChecklist::TAction(ActionIndex);

  if (FTotals[ActionIndex] > 0)
  {
    // if direction is overriden to an action that is otherwise not possible
    // in given synchronization mode, we still want to show the stats
    Possible = true;
  }
  else
  {
    switch (Action)
    {
      case TSynchronizeChecklist::saNone:
        Possible = true;
        break;

      case TSynchronizeChecklist::saUploadNew:
        Possible = ((FMode == smRemote) || (FMode == smBoth)) &&
          FLAGCLEAR(FParams, TTerminal::spTimestamp);
        break;

      case TSynchronizeChecklist::saDownloadNew:
        Possible = ((FMode == smLocal) || (FMode == smBoth)) &&
          FLAGCLEAR(FParams, TTerminal::spTimestamp);
        break;

      case TSynchronizeChecklist::saUploadUpdate:
        Possible =
          ((FMode == smRemote) || (FMode == smBoth)) &&
          (FLAGCLEAR(FParams, TTerminal::spNotByTime) || FLAGSET(FParams, TTerminal::spBySize) || FLAGSET(FParams, TTerminal::spByChecksum));
        break;

      case TSynchronizeChecklist::saDownloadUpdate:
        Possible =
          ((FMode == smLocal) || (FMode == smBoth)) &&
          (FLAGCLEAR(FParams, TTerminal::spNotByTime) || FLAGSET(FParams, TTerminal::spBySize) || FLAGSET(FParams, TTerminal::spByChecksum));
        break;

      case TSynchronizeChecklist::saDeleteRemote:
        Possible = (FMode == smRemote) &&
          FLAGCLEAR(FParams, TTerminal::spTimestamp);
        break;

      case TSynchronizeChecklist::saDeleteLocal:
        Possible = (FMode == smLocal) &&
          FLAGCLEAR(FParams, TTerminal::spTimestamp);
        break;

      default:
        DebugFail();
        Possible = false;
        break;
    }
  }

  UnicodeString PanelText;
  if (Possible)
  {
    PanelText = FORMAT(LoadStrPart(SYNCHRONIZE_SELECTED_ACTIONS, 1),
      (FormatNumber(FChecked[ActionIndex]),
       FormatNumber(FTotals[ActionIndex])));
    if ((FChecked[ActionIndex] > 0) &&
        ((ActionIndex == 0) || !TSynchronizeChecklist::IsItemSizeIrrelevant(Action)))
    {
      PanelText += FORMAT(L" (%s)", (FormatBytes(FCheckedSize[ActionIndex])));
    }
  }
  else
  {
    PanelText = LoadStrPart(SYNCHRONIZE_SELECTED_ACTIONS, 2);
  }

  int TextHeight = StatusBar->Canvas->TextHeight(PanelText);
  int X = Rect.Left + ActionImages->Width + 4;
  int Y = (Rect.Top + Rect.Bottom - TextHeight) / 2;
  StatusBar->Canvas->TextRect(Rect, X, Y, PanelText);

  X = Rect.Left + 1;
  Y = ((Rect.Top + Rect.Bottom - ActionImages->Height) / 2);
  int ImageIndex = ActionIndex;
  ActionImages->Draw(StatusBar->Canvas, X, Y, ImageIndex, Possible);
}
//---------------------------------------------------------------------------
int __fastcall TSynchronizeChecklistDialog::PanelCount()
{
  // last "panel" is technical
  return StatusBar->Panels->Count - 1;
}
//---------------------------------------------------------------------------
int __fastcall TSynchronizeChecklistDialog::PanelAt(int X)
{
  int Result = 0;
  while ((X > StatusBar->Panels->Items[Result]->Width) &&
    (Result < PanelCount()))
  {
    X -= StatusBar->Panels->Items[Result]->Width;
    Result++;
  }

  return ((Result < StatusBar->Panels->Count - 1) ? Result : -1);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::ListViewChange(
  TObject * /*Sender*/, TListItem * Item, TItemChange Change)
{
  if ((Change == ctState) && (FChangingItem == Item) && (FChangingItem != NULL))
  {
    if (!FChangingItemIgnore)
    {
      DebugAssert(Item->Data != NULL);
      if ((FChangingItemChecked != Item->Checked) && (Item->Data != NULL))
      {
        const TSynchronizeChecklist::TItem * ChecklistItem = GetChecklistItem(Item);
        CountItem(ChecklistItem, Item->Checked ? 1 : -1);

        if (!FChangingItemMass)
        {
          UpdateControls();
        }
      }
    }
    FChangingItem = NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::ListViewChanging(
  TObject * /*Sender*/, TListItem * Item, TItemChange Change,
  bool & /*AllowChange*/)
{
  if (Change == ctState)
  {
    FChangingItem = Item;
    FChangingItemChecked = Item->Checked;
  }
  else
  {
    DebugAssert(FChangingItem == NULL);
    FChangingItem = NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::CheckAll(bool Check)
{
  FChangingItemMass = true;
  try
  {
    for (int Index = 0; Index < ListView->Items->Count; Index++)
    {
      ListView->Items->Item[Index]->Checked = Check;
    }
  }
  __finally
  {
    FChangingItemMass = false;
  }
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::CheckAllActionExecute(TObject * /*Sender*/)
{
  CheckAll(true);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::UncheckAllActionExecute(TObject * /*Sender*/)
{
  CheckAll(false);
}
//---------------------------------------------------------------------------
bool TSynchronizeChecklistDialog::IterateItems(TListItem *& Item, TItemStates States)
{
  Item = ListView->GetNextItem(Item, sdAll, States);
  return (Item != NULL);
}
//---------------------------------------------------------------------------
bool TSynchronizeChecklistDialog::IterateSelectedItems(TListItem *& Item)
{
  return IterateItems(Item, TItemStates() << isSelected);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::Check(bool Check)
{
  FChangingItemMass = true;
  try
  {
    TListItem * Item = NULL;
    while (IterateSelectedItems(Item))
    {
      Item->Checked = Check;
    }
  }
  __finally
  {
    FChangingItemMass = false;
  }
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::CheckActionExecute(TObject * /*Sender*/)
{
  Check(true);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::UncheckActionExecute(TObject * /*Sender*/)
{
  Check(false);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::ListViewSelectItem(
  TObject * /*Sender*/, TListItem * /*Item*/, bool /*Selected*/)
{
  // Delayed update of button status in case many items are being selected at once
  // Also change of selection causes buttons to flash, as for short period of time,
  // no item is selected
  UpdateTimer->Enabled = true;
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::UpdateTimerTimer(
  TObject * /*Sender*/)
{
  UpdateTimer->Enabled = false;
  UpdateControls();
}
//---------------------------------------------------------------------------
TListItem * __fastcall TSynchronizeChecklistDialog::SelectAll(bool Select, int Action,
  bool OnlyTheAction)
{
  TListItem * Result = NULL;
  for (int Index = 0; Index < ListView->Items->Count; Index++)
  {
    TListItem * Item = ListView->Items->Item[Index];
    if (Action == 0)
    {
      Item->Selected = Select;
      if (Result == NULL)
      {
        Result = Item;
      }
    }
    else
    {
      const TSynchronizeChecklist::TItem * ChecklistItem = GetChecklistItem(Item);
      bool WantedAction = (int(GetChecklistItemAction(ChecklistItem)) == Action);
      if (WantedAction || !OnlyTheAction)
      {
        Item->Selected = Select && WantedAction;
        if (WantedAction && (Result == NULL))
        {
          Result = Item;
        }
      }
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::SelectAllActionExecute(
  TObject * /*Sender*/)
{
  SelectAll(true);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::StatusBarMouseDown(
  TObject * /*Sender*/, TMouseButton /*Button*/, TShiftState Shift, int X,
  int /*Y*/)
{
  int IPanel = PanelAt(X);

  if (IPanel >= 0)
  {
    TListItem * Item = SelectAll(true, IPanel, Shift.Contains(ssCtrl));
    if (Item != NULL)
    {
      Item->MakeVisible(false);
      Item->Focused = true;
      ListView->SetFocus();
    }
  }
}
//---------------------------------------------------------------------------
TIEListViewColProperties * TSynchronizeChecklistDialog::GetColProperties()
{
  return dynamic_cast<TIEListViewColProperties *>(ListView->ColProperties);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::ListViewCompare(
  TObject * /*Sender*/, TListItem * Item1, TListItem * Item2, int /*Data*/,
  int & Compare)
{
  const TSynchronizeChecklist::TItem * ChecklistItem1 = GetChecklistItem(Item1);
  const TSynchronizeChecklist::TItem * ChecklistItem2 = GetChecklistItem(Item2);

  TIEListViewColProperties * ColProperties = GetColProperties();
  switch (ColProperties->SortColumn)
  {
    case 0: // name
      Compare = AnsiCompareText(ChecklistItem1->GetFileName(), ChecklistItem2->GetFileName());
      break;

    // sorting by local and remote dir is the same
    case 1: // local dir
    case 5: // remote dir
      Compare = 0; // default sorting
      break;

    case 2: // local size
      Compare = CompareNumber(ChecklistItem1->Local.Size, ChecklistItem2->Local.Size);
      break;

    case 3: // local changed
      Compare = CompareFileTime(ChecklistItem1->Local.Modification,
        ChecklistItem2->Local.Modification);
      break;

    case ImageColumnIndex: // action
      Compare = CompareNumber(GetChecklistItemAction(ChecklistItem1), GetChecklistItemAction(ChecklistItem2));
      break;

    case 6: // remote size
      Compare = CompareNumber(ChecklistItem1->Remote.Size, ChecklistItem2->Remote.Size);
      break;

    case 7: // remote changed
      Compare = CompareFileTime(ChecklistItem1->Remote.Modification,
        ChecklistItem2->Remote.Modification);
      break;
  }

  if (Compare == 0)
  {
    Compare = TSynchronizeChecklist::Compare(ChecklistItem1, ChecklistItem2);
  }

  if (!ColProperties->SortAscending)
  {
    Compare = -Compare;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::ListViewSecondaryColumnHeader(
  TCustomIEListView * /*Sender*/, int Index, int & SecondaryColumn)
{
  // "remote dir" column is sorting alias for "local dir" column
  if (Index == 5)
  {
    SecondaryColumn = 1;
  }
  else
  {
    SecondaryColumn = -1;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::ListViewContextPopup(
  TObject * Sender, TPoint & MousePos, bool & Handled)
{
  // to update source popup menu before TBX menu is created
  UpdateControls();
  MenuPopup(Sender, MousePos, Handled);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::CustomCommandsActionExecute(
  TObject * /*Sender*/)
{
  TStrings * LocalFileList = new TStringList();
  TStrings * RemoteFileList = new TStringList();
  try
  {
    TListItem * Item = NULL;
    while (IterateSelectedItems(Item))
    {
      const TSynchronizeChecklist::TItem * ChecklistItem = GetChecklistItem(Item);

      DebugAssert((GetChecklistItemAction(ChecklistItem) == TSynchronizeChecklist::saUploadUpdate) ||
             (GetChecklistItemAction(ChecklistItem) == TSynchronizeChecklist::saDownloadUpdate));
      DebugAssert(ChecklistItem->RemoteFile != NULL);

      UnicodeString LocalPath = ChecklistItem->GetLocalPath();
      LocalFileList->Add(LocalPath);

      UnicodeString RemotePath = ChecklistItem->GetRemotePath();
      RemoteFileList->AddObject(RemotePath, ChecklistItem->RemoteFile);
    }
  }
  catch(...)
  {
    delete LocalFileList;
    delete RemoteFileList;
    throw;
  }

  DebugAssert(FOnCustomCommandMenu != NULL);
  FOnCustomCommandMenu(CustomCommandsAction, LocalFileList, RemoteFileList);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::UpdateStatusBarSize()
{
  int PanelWidth = Min(StatusBar->Width / PanelCount(), ScaleByTextHeight(this, 160));
  for (int Index = 0; Index < PanelCount(); Index++)
  {
    StatusBar->Panels->Items[Index]->Width = PanelWidth;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::StatusBarResize(TObject * /*Sender*/)
{
  UpdateStatusBarSize();
}
//---------------------------------------------------------------------------
const TSynchronizeChecklist::TItem * TSynchronizeChecklistDialog::GetChecklistItem(
  TListItem * Item)
{
  return static_cast<const TSynchronizeChecklist::TItem *>(Item->Data);
}
//---------------------------------------------------------------------------
TSynchronizeChecklist::TAction & TSynchronizeChecklistDialog::GetChecklistItemAction(
  const TSynchronizeChecklist::TItem * ChecklistItem)
{
  TActions::iterator i = FActions.find(ChecklistItem);
  if (i == FActions.end())
  {
    throw EInvalidOperation(L"");
  }
  return i->second;
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::ReverseActionExecute(TObject * /*Sender*/)
{
  TAutoFlag Flag(FChangingItemMass);

  TListItem * Item = NULL;
  while (IterateSelectedItems(Item))
  {
    const TSynchronizeChecklist::TItem * ChecklistItem = GetChecklistItem(Item);
    TSynchronizeChecklist::TAction & Action = GetChecklistItemAction(ChecklistItem);
    TSynchronizeChecklist::TAction NewAction = TSynchronizeChecklist::Reverse(Action);

    if (DebugAlwaysTrue(Action != NewAction))
    {
      CountItemTotal(ChecklistItem, -1);
      if (Item->Checked)
      {
        CountItem(ChecklistItem, -1);
      }

      Action = NewAction;

      CountItemTotal(ChecklistItem, 1);
      if (Item->Checked)
      {
        // item size may differ with action (0 for delete, but non-0 for new file transfer)
        CountItem(ChecklistItem, 1);
      }

      LoadItem(Item);
    }
  }

  Flag.Release();

  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::ListViewClick(TObject * /*Sender*/)
{
  TKeyboardState KeyState;
  GetKeyboardState(KeyState);
  TShiftState ShiftState = KeyboardStateToShiftState(KeyState);
  // when selecting, do not reverse, even when user clicked on action column
  if (!ShiftState.Contains(ssShift) && !ShiftState.Contains(ssCtrl))
  {
    TPoint P = ListView->ScreenToClient(Mouse->CursorPos);
    TRect R = GetColumnHeaderRect(ImageColumnIndex);
    if ((R.Left <= P.x) && (P.x <= R.Right))
    {
      // If no item was selected before the click, the action is not enabled yet here,
      // and Execute would be noop, force update
      UpdateControls();
      ReverseAction->Execute();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::Dispatch(void * AMessage)
{
  TMessage & Message = *reinterpret_cast<TMessage *>(AMessage);
  if (Message.Msg == WM_SYSCOMMAND)
  {
    if (!HandleMinimizeSysCommand(Message))
    {
      TForm::Dispatch(AMessage);
    }
  }
  else if (Message.Msg == WM_WANTS_MOUSEWHEEL_INACTIVE)
  {
    Message.Result = FSynchronizing ? 1 : 0;
  }
  else if (Message.Msg == WM_MANAGES_CAPTION)
  {
    // calling UpdateControls would cause status bar flicker
    UpdateCaption();
    Message.Result = 1;
  }
  else
  {
    TForm::Dispatch(AMessage);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::UpdateImages()
{
  ListView->SmallImages = ShellImageListForControl(this, ilsSmall);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::FormAfterMonitorDpiChanged(TObject *, int OldDPI, int NewDPI)
{
  DebugUsedParam2(OldDPI, NewDPI);
  UpdateImages();
  UpdateStatusBarSize();
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::ProcessedItem(void * /*Token*/, const TSynchronizeChecklist::TItem * ChecklistItem)
{
  TListItem * Item = FChecklistToListViewMap[ChecklistItem];
  DebugAssert(Item->Checked);
  Item->Checked = false;
  Item->MakeVisible(false);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::UpdatedSynchronizationChecklistItems(
  const TSynchronizeChecklist::TItemList & Items)
{
  TSynchronizeChecklist::TItemList::const_iterator Iter = Items.begin();
  while (Iter != Items.end())
  {
    const TSynchronizeChecklist::TItem * ChecklistItem = *Iter;
    TListItem * Item = FChecklistToListViewMap[ChecklistItem];
    LoadItem(Item);
    // When called from FOnSynchronize, we rely on a caller never to update size of an item that had size already,
    // otherwise we get it counted twice here.
    if (Item->Checked)
    {
      CountItemSize(ChecklistItem, 1);
    }
    Iter++;
  }
  UpdateControls();
}
//---------------------------------------------------------------------------
void TSynchronizeChecklistDialog::DoSynchronize(bool Queue)
{
  ListView->SelectAll(smNone);
  for (int Index = 0; Index < ListView->Items->Count; Index++)
  {
    TListItem * Item = ListView->Items->Item[Index];
    const TSynchronizeChecklist::TItem * ChecklistItem = GetChecklistItem(Item);
    FChecklist->Update(ChecklistItem, Item->Checked, GetChecklistItemAction(ChecklistItem));
  }

  TAutoFlag Flag(FSynchronizing);
  UpdateControls();
  try
  {
    if (Queue)
    {
      FOnQueueSynchronize(FToken);
    }
    else
    {
      FOnSynchronize(FToken, ProcessedItem, UpdatedSynchronizationChecklistItems);
    }
  }
  catch (Exception & E)
  {
    FException.reset(CloneException(&E));
  }

  // Needed when called from the drop down menu
  ModalResult = OkButton->ModalResult;
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::OkButtonClick(TObject *)
{
  bool Queue = OpenInNewWindow();
  if (Queue && (FOnQueueSynchronize == NULL))
  {
    Beep();
  }
  else
  {
    DoSynchronize(Queue);
  }
}
//---------------------------------------------------------------------------
void TSynchronizeChecklistDialog::CalculateSize(bool All)
{
  TItemStates States;
  if (!All)
  {
    States << isSelected;
  }
  TSynchronizeChecklist::TItemList Items;
  TListItem * Item = NULL;
  while (IterateItems(Item, States))
  {
    const TSynchronizeChecklist::TItem * ChecklistItem = GetChecklistItem(Item);
    Items.push_back(ChecklistItem);
    if (Item->Checked)
    {
      CountItemSize(ChecklistItem, -1);
    }
  }

  try
  {
    FOnSynchronizeChecklistCalculateSize(FChecklist, Items, FToken);
  }
  __finally
  {
    UpdatedSynchronizationChecklistItems(Items);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::CalculateSizeActionExecute(TObject *)
{
  CalculateSize(false);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::CalculateSizeAllActionExecute(TObject *)
{
  CalculateSize(true);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::DeleteItem(const TSynchronizeChecklist::TItem * ChecklistItem)
{
  TListItem * Item = FChecklistToListViewMap[ChecklistItem];
  CountItemTotal(ChecklistItem, -1);
  if (Item->Checked)
  {
    CountItem(ChecklistItem, -1);
  }

  FActions.erase(ChecklistItem);
  FChecklistToListViewMap.erase(ChecklistItem);

  FChecklist->Delete(ChecklistItem);
  int Index = Item->Index;
  if (Item->Focused)
  {
    int FocusIndex = Index;
    if (FocusIndex == ListView->Items->Count - 1)
    {
      FocusIndex--;
    }
    else
    {
      FocusIndex++;
    }
    ListView->ItemFocused = ListView->Items->Item[FocusIndex];
  }
  ListView->Items->Delete(Index);
  FMoveCandidatesValidForSort = MaxInt; // can be optimized
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::MoveActionExecute(TObject *)
{
  TMoveData MoveData(true);
  TListItem * Item = NULL;
  while (IterateSelectedItems(Item))
  {
    const TSynchronizeChecklist::TItem * ChecklistItem = GetChecklistItem(Item);
    TSynchronizeChecklist::TAction Action = GetChecklistItemAction(ChecklistItem);
    MoveData.Collect(ChecklistItem, Action);
  }

  TMoveActionData * TransferAction = &MoveData.FirstAction;
  TMoveActionData * DeleteAction = &MoveData.SecondAction;
  if (IsTransferNewAction(DeleteAction->Action))
  {
    std::swap(TransferAction, DeleteAction);
  }
  DebugAssert(IsTransferNewAction(TransferAction->Action));
  DebugAssert(GetOppositeMoveAction(DeleteAction->Action) == TransferAction->Action);

  TOperationSide Side;
  UnicodeString NewFileName;
  DebugAssert(TransferAction->ChecklistItems.size() == 1);
  const TSynchronizeChecklist::TItem * TransferChecklistItem = TransferAction->ChecklistItems[0];
  if (DeleteAction->Action == TSynchronizeChecklist::saDeleteRemote)
  {
    Side = osRemote;
    NewFileName = UnixCombinePaths(TransferChecklistItem->Remote.Directory, TransferChecklistItem->Local.FileName);
  }
  else if (DebugAlwaysTrue(DeleteAction->Action == TSynchronizeChecklist::saDeleteLocal))
  {
    Side = osLocal;
    NewFileName = CombinePaths(TransferChecklistItem->Local.Directory, TransferChecklistItem->Remote.FileName);
  }

  if (DebugAlwaysTrue(!NewFileName.IsEmpty()))
  {
    bool Move = false;
    bool TargetIsDirectory;
    if ((TransferAction->AllItemsDirectories == DeleteAction->AllItemsDirectories) &&
        DebugAlwaysTrue(TransferAction->ItemsCount == 1) &&
        DebugAlwaysTrue(DeleteAction->ItemsCount == 1))
    {
      Move = true;
      TargetIsDirectory = false;
    }
    else if (DebugAlwaysTrue(TransferAction->AllItemsDirectories && (TransferAction->ItemsCount == 1)))
    {
      TargetIsDirectory = true;
      Move = true;
    }

    if (DebugAlwaysTrue(Move))
    {
      FOnSynchronizeMove(Side, DeleteAction->FileList.get(), NewFileName, TargetIsDirectory, FToken);

      TMoveActionData::TChecklistItems::const_iterator I = DeleteAction->ChecklistItems.begin();
      while (I != DeleteAction->ChecklistItems.end())
      {
        DeleteItem(*I);
        I++;
      }
      if (!TargetIsDirectory)
      {
        DeleteItem(TransferChecklistItem);
      }
      else
      {
        // The remaning "transfer" item
        ListView->Selected->MakeVisible(false);
      }
      UpdateControls();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::CheckDirectory(bool Check)
{
  std::unique_ptr<TStringList> Directories(new TStringList());
  TListItem * Item = NULL;
  while (IterateSelectedItems(Item))
  {
    const TSynchronizeChecklist::TItem * ChecklistItem = GetChecklistItem(Item);
    // It does not matter if we use local or remote directory
    Directories->Add(IncludeTrailingBackslash(ChecklistItem->Local.Directory));
  }

  TAutoFlag ChangingItemMassSwitch(FChangingItemMass);
  for (int Index = 0; Index < ListView->Items->Count; Index++)
  {
    TListItem * Item = ListView->Items->Item[Index];
    const TSynchronizeChecklist::TItem * ChecklistItem = GetChecklistItem(Item);
    UnicodeString Directory = IncludeTrailingBackslash(ChecklistItem->Local.Directory);
    for (int Index2 = 0; Index2 < Directories->Count; Index2++)
    {
      if (StartsText(Directories->Strings[Index2], Directory))
      {
        Item->Checked = Check;
      }
    }
  }
  ChangingItemMassSwitch.Release();
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::CheckDirectoryActionExecute(TObject *)
{
  CheckDirectory(true);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::UncheckDirectoryActionExecute(TObject *)
{
  CheckDirectory(false);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::DoBrowse(TOperationSide Side)
{
  const TSynchronizeChecklist::TItem * ChecklistItem = GetChecklistItem(ListView->Selected);
  TSynchronizeChecklist::TAction Action = GetChecklistItemAction(ChecklistItem);
  FOnSynchronizeBrowse(Side, Action, ChecklistItem);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::BrowseLocalActionExecute(TObject *)
{
  DoBrowse(osLocal);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::BrowseRemoteActionExecute(TObject *)
{
  DoBrowse(osRemote);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::KeyDown(Word & Key, TShiftState Shift)
{
  TShortCut KeyShortCut = ShortCut(Key, Shift);
  TShortCut CustomShortCut = NormalizeCustomShortCut(KeyShortCut);
  if (IsCustomShortCut(CustomShortCut))
  {
    TTBXItem * MenuItem = new TTBXItem(this);
    CustomCommandsAction->ActionComponent = MenuItem;
    CustomCommandsAction->Execute();

    for (int Index = 0; Index < MenuItem->Count; Index++)
    {
      TTBCustomItem * Item = MenuItem->Items[Index];
      if (Item->Enabled && (Item->ShortCut == CustomShortCut))
      {
        TAutoFlag DontCopyCommandToClipboardFlag(DontCopyCommandToClipboard);
        Item->Click();
        Key = 0;
        break;
      }
    }
  }

  TForm::KeyDown(Key, Shift);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::ListViewRecreate(TObject *)
{
  FChecklistToListViewMap.clear();
  for (int Index = 0; Index < ListView->Items->Count; Index++)
  {
    TListItem * Item = ListView->Items->Item[Index];
    const TSynchronizeChecklist::TItem * ChecklistItem = GetChecklistItem(Item);
    FChecklistToListViewMap.insert(std::make_pair(ChecklistItem, Item));
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::ToolsMenuButtonClick(TObject *)
{
  MenuPopup(ToolsPopupMenu, ToolsMenuButton);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::FindMoveCandidateActionExecute(TObject *)
{
  TIEListViewColProperties * ColProperties = GetColProperties();
  int Sort = (ColProperties->SortAscending ? 1 : -1) * ColProperties->SortColumn;

  if (FMoveCandidatesValidForSort != Sort)
  {
    FMoveCandidatesFileName.clear();
    FMoveCandidatesSize.clear();
    for (int Index = 0; Index < ListView->Items->Count; Index++)
    {
      const TSynchronizeChecklist::TItem * ChecklistItem = GetChecklistItem(ListView->Items->Item[Index]);
      if (ChecklistItem->IsLocalOnly() || ChecklistItem->IsRemoteOnly())
      {
        if (ChecklistItem->IsDirectory)
        {
          FMoveCandidatesFileName[ChecklistItem->GetFileName().LowerCase()].push_back(ChecklistItem);
        }
        else
        {
          FMoveCandidatesSize[ChecklistItem->GetBaseSize()].push_back(ChecklistItem);
        }
      }
    }

    FMoveCandidatesValidForSort = Sort;
  }

  bool Found = false;
  if (!FMoveCandidatesFileName.empty() || !FMoveCandidatesSize.empty())
  {
    TListItem * ItemFocused = ListView->ItemFocused;
    TListItem * Item;
    if (ItemFocused != NULL)
    {
      Item = ItemFocused;
    }
    // Can we have a selection without focus?
    else if (DebugAlwaysFalse(ListView->Selected != NULL))
    {
      Item = ListView->Selected;
    }
    else if (ListView->Items->Count > 0)
    {
      Item = ListView->Items->Item[0];
    }
    else
    {
      DebugFail(); // the action is disabled when there are no items
      Item = NULL;
    }

    TListItem * FirstItem = NULL;

    while (!Found && (Item != NULL))
    {
      const TSynchronizeChecklist::TItem * ChecklistItem = GetChecklistItem(Item);
      TSynchronizeChecklist::TAction Action = GetChecklistItemAction(ChecklistItem);
      TSynchronizeChecklist::TAction OppositeAction = GetOppositeMoveAction(Action);
      if ((OppositeAction != TSynchronizeChecklist::saNone) &&
          // For focused item, we search pair even if the focused item is "delete" action,
          // but when searching the next items, consider "transfer" actions only
          ((Item == ItemFocused) || IsTransferNewAction(Action)))
      {
        TChecklistItems Candidates;
        if (ChecklistItem->IsDirectory)
        {
          UnicodeString FileName = ChecklistItem->GetFileName().LowerCase();
          TMoveCandidatesFileNameMap::const_iterator I = FMoveCandidatesFileName.find(FileName);
          if (I != FMoveCandidatesFileName.end())
          {
            const TChecklistItems & NameCandidates = I->second;
            for (size_t I = 0; I < NameCandidates.size(); I++)
            {
              const TSynchronizeChecklist::TItem * ChecklistItem2 = NameCandidates[I];
              if ((GetChecklistItemAction(ChecklistItem2) == OppositeAction) &&
                  DebugAlwaysTrue(ChecklistItem2->IsDirectory))
              {
                Candidates.push_back(ChecklistItem2);
              }
            }
          }
        }
        else
        {
          __int64 Size = ChecklistItem->GetBaseSize();
          TMoveCandidatesSizeMap::const_iterator I = FMoveCandidatesSize.find(Size);
          if (I != FMoveCandidatesSize.end())
          {
            UnicodeString FileName = ChecklistItem->GetFileName();
            const TChecklistItems & SizeCandidates = I->second;
            for (size_t I = 0; I < SizeCandidates.size(); I++)
            {
              const TSynchronizeChecklist::TItem * ChecklistItem2 = SizeCandidates[I];
              if ((GetChecklistItemAction(ChecklistItem2) == OppositeAction) &&
                  DebugAlwaysTrue(!ChecklistItem2->IsDirectory))
              {
                bool IsCandidate;
                // in addition to the same size, also the same file filename (although possibly different directory)
                if (SameText(FileName, ChecklistItem2->GetFileName()))
                {
                  IsCandidate = true;
                }
                // or different filename but the same directory (in addition to the same size)
                else if ((Action == TSynchronizeChecklist::saDeleteLocal) ||
                         (Action == TSynchronizeChecklist::saDownloadNew))
                {
                  IsCandidate = SamePaths(ChecklistItem->Local.Directory, ChecklistItem2->Local.Directory);
                }
                else if ((Action == TSynchronizeChecklist::saDeleteRemote) ||
                         (Action == TSynchronizeChecklist::saUploadNew))
                {
                  IsCandidate = UnixSamePath(ChecklistItem->Remote.Directory, ChecklistItem2->Remote.Directory);
                }
                else
                {
                  DebugFail();
                  IsCandidate = false;
                }

                if (IsCandidate)
                {
                  Candidates.push_back(ChecklistItem2);
                }
              }
            }
          }
        }

        if (!Candidates.empty())
        {
          const TSynchronizeChecklist::TItem * ChecklistItem2 = Candidates[0];
          if ((FirstItem == NULL) && Item->Selected && (ListView->SelCount == 2))
          {
            TListItem * NextSelected = ListView->GetNextItem(Item, sdAll, TItemStates() << isSelected);
            if (NextSelected == NULL)
            {
              NextSelected = ListView->Selected; // Shorthand for GetNextItem(NULL, sdAll, isSelected)
            }
            TChecklistItems::const_iterator I = std::find(Candidates.begin(), Candidates.end(), GetChecklistItem(NextSelected));
            if (I < Candidates.end() - 1)
            {
              ChecklistItem2 = *(I + 1);
            }
            else if (I == Candidates.end() - 1)
            {
              ChecklistItem2 = NULL;
            }
          }

          if (ChecklistItem2 != NULL)
          {
            TListItem * Item2 = FChecklistToListViewMap[ChecklistItem2];

            for (int Index = 0; Index < ListView->Items->Count; Index++)
            {
              TListItem * ItemI = ListView->Items->Item[Index];
              ItemI->Selected = (ItemI == Item) || (ItemI == Item2);
            }
            ListView->ItemFocused = Item;

            // IsItemVisible returns true even on partial visibility,
            // so it is still worth trying MakeVisible to make them completelly visible
            bool FlickerExpected = !ListView->IsItemVisible(Item) || !ListView->IsItemVisible(Item2);
            if (FlickerExpected)
            {
              // does not seem to have any effect
              ListView->LockDrawing();
            }
            try
            {
              // even if the item turns to be invisible in the end, it at least scrolls the view towards it
              Item2->MakeVisible(false);
              // Should do minimal scroll needed to make the first item whole visible, but not hiding the second
              Item->MakeVisible(false);
            }
            __finally
            {
              if (FlickerExpected)
              {
                ListView->UnlockDrawing();
              }
            }

            Found = true;
          }
        }
      }

      // Allow going through the first item twice, to make sure we roll over its previous pair candidates
      if (Item == FirstItem)
      {
        Item = NULL;
      }
      else
      {
        if (FirstItem == NULL)
        {
          FirstItem = Item;
        }

        Item = ListView->GetNextItem(Item, sdAll, TItemStates());
        if (Item == NULL)
        {
          Item = ListView->Items->Item[0];
        }
      }
    }
  }

  if (!Found)
  {
    MessageBeep(MB_ICONHAND);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::StartItemClick(TObject *)
{
  DoSynchronize(false);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::OkButtonDropDownClick(TObject *)
{
  MenuPopup(OkPopupMenu, OkButton);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::StartQueueItemClick(TObject *)
{
  DoSynchronize(true);
}
//---------------------------------------------------------------------------
