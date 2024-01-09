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
  TSynchronizeChecklistCalculateSize OnSynchronizeChecklistCalculateSize, TSynchronizeMoveEvent OnSynchronizeMove,
  TSynchronizeBrowseEvent OnSynchronizeBrowse, void * Token)
{
  std::unique_ptr<TSynchronizeChecklistDialog> Dialog(
    new TSynchronizeChecklistDialog(
      Application, Mode, Params, LocalDirectory, RemoteDirectory, OnCustomCommandMenu, OnSynchronize,
      OnSynchronizeChecklistCalculateSize, OnSynchronizeMove, OnSynchronizeBrowse, Token));
  return Dialog->Execute(Checklist);
}
//---------------------------------------------------------------------
__fastcall TSynchronizeChecklistDialog::TSynchronizeChecklistDialog(
  TComponent * AOwner, TSynchronizeMode Mode, int Params,
  const UnicodeString & LocalDirectory, const UnicodeString & RemoteDirectory,
  TCustomCommandMenuEvent OnCustomCommandMenu, TFullSynchronizeEvent OnSynchronize,
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
  FToken = Token;
  UseSystemSettings(this);
  UseDesktopFont(ListView);
  UseDesktopFont(StatusBar);
  FChecklist = NULL;
  FChangingItem = NULL;
  FChangingItemIgnore = false;
  FChangingItemMass = false;
  FSynchronizing = false;

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
  TListItem * Item = ListView->Selected;
  while (Item != NULL)
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
    Item = ListView->GetNextItem(Item, sdAll, TItemStates() << isSelected);
  }

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
  ReverseAction->Enabled = (ListView->SelCount > 0) && DebugAlwaysTrue(!FSynchronizing);
  MoveAction->Enabled = (GetMoveItems() != TSynchronizeMoveItems());
  CalculateSizeAction->Enabled = (ListView->SelCount > 0) && AnyDirectory && DebugAlwaysTrue(!FSynchronizing);
  TSynchronizeChecklist::TAction SelectedItemAction =
    (ListView->SelCount == 1) ? GetChecklistItemAction(GetChecklistItem(ListView->Selected)) : TSynchronizeChecklist::saNone;
  BrowseLocalAction->Enabled = (ListView->SelCount == 1) && (SelectedItemAction != TSynchronizeChecklist::saDeleteRemote);
  BrowseRemoteAction->Enabled = (ListView->SelCount == 1) && (SelectedItemAction != TSynchronizeChecklist::saDeleteLocal);

  SelectAllAction->Enabled = (ListView->SelCount < ListView->Items->Count) && !FSynchronizing;
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
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::LoadList()
{
  memset(&FTotals, 0, sizeof(FTotals));
  memset(&FChecked, 0, sizeof(FChecked));
  memset(&FCheckedSize, 0, sizeof(FCheckedSize));

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
          (FLAGCLEAR(FParams, TTerminal::spNotByTime) || FLAGSET(FParams, TTerminal::spBySize));
        break;

      case TSynchronizeChecklist::saDownloadUpdate:
        Possible =
          ((FMode == smLocal) || (FMode == smBoth)) &&
          (FLAGCLEAR(FParams, TTerminal::spNotByTime) || FLAGSET(FParams, TTerminal::spBySize));
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
void __fastcall TSynchronizeChecklistDialog::Check(bool Check)
{
  FChangingItemMass = true;
  try
  {
    TListItem * Item = ListView->Selected;
    while (Item != NULL)
    {
      Item->Checked = Check;
      Item = ListView->GetNextItem(Item, sdAll, TItemStates() << isSelected);
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
void __fastcall TSynchronizeChecklistDialog::ListViewCompare(
  TObject * /*Sender*/, TListItem * Item1, TListItem * Item2, int /*Data*/,
  int & Compare)
{
  const TSynchronizeChecklist::TItem * ChecklistItem1 = GetChecklistItem(Item1);
  const TSynchronizeChecklist::TItem * ChecklistItem2 = GetChecklistItem(Item2);

  TIEListViewColProperties * ColProperties =
    dynamic_cast<TIEListViewColProperties *>(ListView->ColProperties);

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
    if (!ChecklistItem1->Local.Directory.IsEmpty())
    {
      Compare = AnsiCompareText(ChecklistItem1->Local.Directory, ChecklistItem2->Local.Directory);
    }
    else
    {
      DebugAssert(!ChecklistItem1->Remote.Directory.IsEmpty());
      Compare = AnsiCompareText(ChecklistItem1->Remote.Directory, ChecklistItem2->Remote.Directory);
    }

    if (Compare == 0)
    {
      Compare = AnsiCompareText(ChecklistItem1->GetFileName(), ChecklistItem2->GetFileName());
    }
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
    TListItem * Item = ListView->Selected;
    DebugAssert(Item != NULL);

    while (Item != NULL)
    {
      const TSynchronizeChecklist::TItem * ChecklistItem = GetChecklistItem(Item);

      DebugAssert((GetChecklistItemAction(ChecklistItem) == TSynchronizeChecklist::saUploadUpdate) ||
             (GetChecklistItemAction(ChecklistItem) == TSynchronizeChecklist::saDownloadUpdate));
      DebugAssert(ChecklistItem->RemoteFile != NULL);

      UnicodeString LocalPath =
        IncludeTrailingBackslash(ChecklistItem->Local.Directory) +
        ChecklistItem->Local.FileName;

      LocalFileList->Add(LocalPath);

      UnicodeString RemotePath =
        UnixIncludeTrailingBackslash(ChecklistItem->Remote.Directory) +
        ChecklistItem->Remote.FileName;

      RemoteFileList->AddObject(RemotePath, ChecklistItem->RemoteFile);

      Item = ListView->GetNextItem(Item, sdAll, TItemStates() << isSelected);
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

  TListItem * Item = ListView->Selected;
  while (Item != NULL)
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

    Item = ListView->GetNextItem(Item, sdAll, TItemStates() << isSelected);
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
  else if (Message.Msg == CM_DPICHANGED)
  {
    CMDpiChanged(Message);
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
void __fastcall TSynchronizeChecklistDialog::CMDpiChanged(TMessage & Message)
{
  TForm::Dispatch(&Message);
  UpdateImages();
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
void __fastcall TSynchronizeChecklistDialog::OkButtonClick(TObject * /*Sender*/)
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
    FOnSynchronize(FToken, ProcessedItem, UpdatedSynchronizationChecklistItems);
  }
  catch (Exception & E)
  {
    FException.reset(CloneException(&E));
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::CalculateSizeActionExecute(TObject * /*Sender*/)
{
  TItemStates States;
  if (!IsKeyPressed(VK_CONTROL))
  {
    States << isSelected;
  }
  TSynchronizeChecklist::TItemList Items;
  TListItem * Item = NULL;
  while ((Item = ListView->GetNextItem(Item, sdAll, States)) != NULL)
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
void __fastcall TSynchronizeChecklistDialog::CalculateSizeAllActionExecute(TObject * Sender)
{
  DebugAssert(IsKeyPressed(VK_CONTROL));
  CalculateSizeActionExecute(Sender);
}
//---------------------------------------------------------------------------
TSynchronizeChecklistDialog::TSynchronizeMoveItems __fastcall TSynchronizeChecklistDialog::GetMoveItems()
{
  if ((ListView->SelCount != 2) || DebugAlwaysFalse(FSynchronizing))
  {
    return TSynchronizeMoveItems();
  }
  else
  {
    TListItem * Item1 = ListView->Selected;
    const TSynchronizeChecklist::TItem * ChecklistItem1 = GetChecklistItem(DebugNotNull(Item1));
    TListItem * Item2 = ListView->GetNextItem(ListView->Selected, sdAll, TItemStates() << isSelected);
    const TSynchronizeChecklist::TItem * ChecklistItem2 = GetChecklistItem(DebugNotNull(Item2));

    if (ChecklistItem1->IsDirectory != ChecklistItem2->IsDirectory)
    {
      return TSynchronizeMoveItems();
    }
    else
    {
      TSynchronizeChecklist::TAction Action1 = GetChecklistItemAction(ChecklistItem1);
      TSynchronizeChecklist::TAction Action2 = GetChecklistItemAction(ChecklistItem2);

      if ((Action1 == TSynchronizeChecklist::saUploadNew) && (Action2 == TSynchronizeChecklist::saDeleteRemote))
      {
        return TSynchronizeMoveItems(ChecklistItem1, ChecklistItem2);
      }
      else if ((Action1 == TSynchronizeChecklist::saDownloadNew) && (Action2 == TSynchronizeChecklist::saDeleteLocal))
      {
        return TSynchronizeMoveItems(ChecklistItem1, ChecklistItem2);
      }
      else if ((Action1 == TSynchronizeChecklist::saDeleteRemote) && (Action2 == TSynchronizeChecklist::saUploadNew))
      {
        return TSynchronizeMoveItems(ChecklistItem2, ChecklistItem1);
      }
      else if ((Action1 == TSynchronizeChecklist::saDeleteLocal) && (Action2 == TSynchronizeChecklist::saDownloadNew))
      {
        return TSynchronizeMoveItems(ChecklistItem2, ChecklistItem1);
      }
      else
      {
        return TSynchronizeMoveItems();
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::DeleteItem(TListItem * Item)
{
  const TSynchronizeChecklist::TItem * ChecklistItem = GetChecklistItem(Item);
  CountItemTotal(ChecklistItem, -1);
  if (Item->Checked)
  {
    CountItem(ChecklistItem, -1);
  }

  FActions.erase(ChecklistItem);
  FChecklistToListViewMap.erase(ChecklistItem);

  FChecklist->Delete(ChecklistItem);
  ListView->Items->Delete(Item->Index);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::MoveActionExecute(TObject *)
{
  TSynchronizeMoveItems MoveItems = GetMoveItems();
  TSynchronizeChecklist::TAction Action2 = GetChecklistItemAction(MoveItems.second);

  TOperationSide Side;
  UnicodeString FileName;
  UnicodeString NewFileName;
  TRemoteFile * RemoteFile;
  if (Action2 == TSynchronizeChecklist::saDeleteRemote)
  {
    Side = osRemote;
    FileName = UnixCombinePaths(MoveItems.second->Remote.Directory, MoveItems.second->Remote.FileName);
    NewFileName = UnixCombinePaths(MoveItems.first->Remote.Directory, MoveItems.first->Local.FileName);
    RemoteFile = MoveItems.second->RemoteFile;
  }
  else if (Action2 == TSynchronizeChecklist::saDeleteLocal)
  {
    Side = osLocal;
    FileName = TPath::Combine(MoveItems.second->Local.Directory, MoveItems.second->Local.FileName);
    NewFileName = TPath::Combine(MoveItems.first->Local.Directory, MoveItems.first->Remote.FileName);
    RemoteFile = NULL;
  }
  else
  {
    DebugFail();
    Abort();
  }

  FOnSynchronizeMove(Side, FileName, NewFileName, RemoteFile);

  TListItem * Item1 = DebugNotNull(ListView->FindData(0, const_cast<TSynchronizeChecklist::TItem *>(MoveItems.first), true, false));
  TListItem * Item2 = DebugNotNull(ListView->FindData(0, const_cast<TSynchronizeChecklist::TItem *>(MoveItems.second), true, false));

  DeleteItem(Item1);
  DeleteItem(Item2);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::CheckDirectory(bool Check)
{
  std::unique_ptr<TStringList> Directories(new TStringList());
  TListItem * Item = ListView->Selected;
  while (Item != NULL)
  {
    const TSynchronizeChecklist::TItem * ChecklistItem = GetChecklistItem(Item);
    // It does not matter if we use local or remote directory
    Directories->Add(IncludeTrailingBackslash(ChecklistItem->Local.Directory));
    Item = ListView->GetNextItem(Item, sdAll, TItemStates() << isSelected);
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
