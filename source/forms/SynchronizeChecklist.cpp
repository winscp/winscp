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
//---------------------------------------------------------------------
#pragma link "IEListView"
#pragma link "NortonLikeListView"
#pragma link "PngImageList"
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------
const int ImageColumnIndex = 4;
//---------------------------------------------------------------------
bool __fastcall DoSynchronizeChecklistDialog(TSynchronizeChecklist * Checklist,
  TSynchronizeMode Mode, int Params, const UnicodeString LocalDirectory,
  const UnicodeString RemoteDirectory, TCustomCommandMenuEvent OnCustomCommandMenu)
{
  bool Result;
  TSynchronizeChecklistDialog * Dialog = new TSynchronizeChecklistDialog(
    Application, Mode, Params, LocalDirectory, RemoteDirectory, OnCustomCommandMenu);
  try
  {
    Result = Dialog->Execute(Checklist);
  }
  __finally
  {
    delete Dialog;
  }
  return Result;
}
//---------------------------------------------------------------------
__fastcall TSynchronizeChecklistDialog::TSynchronizeChecklistDialog(
  TComponent * AOwner, TSynchronizeMode Mode, int Params,
  const UnicodeString LocalDirectory, const UnicodeString RemoteDirectory,
  TCustomCommandMenuEvent OnCustomCommandMenu)
  : TForm(AOwner)
{
  FFormRestored = false;
  FMode = Mode;
  FParams = Params;
  FLocalDirectory = ExcludeTrailingBackslash(LocalDirectory);
  FRemoteDirectory = UnixExcludeTrailingBackslash(RemoteDirectory);
  FOnCustomCommandMenu = OnCustomCommandMenu;
  UseSystemSettings(this);
  UseDesktopFont(ListView);
  UseDesktopFont(StatusBar);
  FChecklist = NULL;
  FChangingItem = NULL;
  FChangingItemIgnore = false;
  FChangingItemMass = false;
  FGeneralHint = StatusBar->Hint;

  SelectScaledImageList(ActionImages);

  FOrigListViewWindowProc = ListView->WindowProc;
  ListView->WindowProc = ListViewWindowProc;

  FSystemImageList = SharedSystemImageList(false);
  ListView->SmallImages = FSystemImageList;

  CustomCommandsAction->Visible = (FOnCustomCommandMenu != NULL);
  // button visibility cannot be bound to action visibility
  CustomCommandsButton2->Visible = CustomCommandsAction->Visible;
  MenuButton(CustomCommandsButton2);
}
//---------------------------------------------------------------------
__fastcall TSynchronizeChecklistDialog::~TSynchronizeChecklistDialog()
{
  delete FSystemImageList;
  ListView->WindowProc = FOrigListViewWindowProc;
}
//---------------------------------------------------------------------
bool __fastcall TSynchronizeChecklistDialog::Execute(TSynchronizeChecklist * Checklist)
{
  FChecklist = Checklist;

  bool Result = (ShowModal() == DefaultResult(this));

  if (Result)
  {
    for (int Index = 0; Index < ListView->Items->Count; Index++)
    {
      TListItem * Item = ListView->Items->Item[Index];
      const TSynchronizeChecklist::TItem * ChecklistItem = GetChecklistItem(Item);
      Checklist->Update(ChecklistItem, Item->Checked, GetChecklistItemAction(ChecklistItem));
    }

    TSynchronizeChecklistConfiguration FormConfiguration =
      CustomWinConfiguration->SynchronizeChecklist;
    FormConfiguration.ListParams = ListView->ColProperties->ParamsStr;

    UnicodeString WindowParams = FormConfiguration.WindowParams;
    // if there is no main window, keep previous "custom pos" indication,
    bool CustomPos = (StrToIntDef(CutToChar(WindowParams, L';', true), 0) != 0);
    if (Application->MainForm != NULL)
    {
      CustomPos = (Application->MainForm->BoundsRect != BoundsRect);
    }
    FormConfiguration.WindowParams =
      FORMAT(L"%d;%s", ((CustomPos ? 1 : 0), StoreForm(this)));

    CustomWinConfiguration->SynchronizeChecklist = FormConfiguration;
  }

  return Result;
}
//---------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::UpdateControls()
{
  StatusBar->Invalidate();

  bool AllChecked = true;
  bool AllUnchecked = true;
  bool AnyBoth = false;
  bool AnyNonBoth = false;
  TListItem * Item = ListView->Selected;
  while (Item != NULL)
  {
    TSynchronizeChecklist::TAction Action = GetChecklistItemAction(GetChecklistItem(Item));
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
    Item = ListView->GetNextItem(Item, sdAll, TItemStates() << isSelected);
  }

  EnableControl(OkButton, (FChecked[0] > 0));
  CheckAction->Enabled = !AllChecked;
  UncheckAction->Enabled = !AllUnchecked;
  CheckAllAction->Enabled = (FChecked[0] < FTotals[0]);
  UncheckAllAction->Enabled = (FChecked[0] > 0);
  CustomCommandsAction->Enabled = AnyBoth && !AnyNonBoth;
  ReverseAction->Enabled = (ListView->SelCount > 0);

  SelectAllAction->Enabled = (ListView->SelCount < ListView->Items->Count);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::CreateParams(TCreateParams & Params)
{
  if (!FFormRestored)
  {
    FFormRestored = True;
    UnicodeString WindowParams = CustomWinConfiguration->SynchronizeChecklist.WindowParams;
    bool CustomPos = (StrToIntDef(CutToChar(WindowParams, L';', true), 0) != 0);

    if (!CustomPos && (Application->MainForm != NULL))
    {
      BoundsRect = Application->MainForm->BoundsRect;
    }
    else
    {
      RestoreForm(WindowParams, this);
    }
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
      if (ChecklistItem->IsDirectory)
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
      if (ChecklistItem->IsDirectory)
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
void __fastcall TSynchronizeChecklistDialog::LoadList()
{
  memset(&FTotals, 0, sizeof(FTotals));
  memset(&FChecked, 0, sizeof(FChecked));
  memset(&FCheckedSize, 0, sizeof(FCheckedSize));
  FTotals[0] = FChecklist->Count;

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
        Item->Data = const_cast<TSynchronizeChecklist::TItem *>(ChecklistItem);
        Item->Checked = ChecklistItem->Checked;
        LoadItem(Item);
      }
      __finally
      {
        FChangingItemIgnore = false;
      }
      int ActionIndex = int(GetChecklistItemAction(ChecklistItem));
      FTotals[ActionIndex]++;
      if (ChecklistItem->Checked)
      {
        FChecked[ActionIndex]++;
        FChecked[0]++;

        __int64 ItemSize = GetItemSize(ChecklistItem);
        FCheckedSize[ActionIndex] += ItemSize;
        FCheckedSize[0] += ItemSize;
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
bool __fastcall TSynchronizeChecklistDialog::IsItemSizeIrrelevant(TSynchronizeChecklist::TAction Action)
{
  switch (Action)
  {
    case TSynchronizeChecklist::saNone:
    case TSynchronizeChecklist::saDeleteRemote:
    case TSynchronizeChecklist::saDeleteLocal:
      return true;

    default:
      return false;
  }
}
//---------------------------------------------------------------------------
__int64 __fastcall TSynchronizeChecklistDialog::GetItemSize(const TSynchronizeChecklist::TItem * Item)
{
  TSynchronizeChecklist::TAction Action = GetChecklistItemAction(Item);
  if (IsItemSizeIrrelevant(Action))
  {
    return 0;
  }
  else
  {
    switch (Action)
    {
      case TSynchronizeChecklist::saUploadNew:
      case TSynchronizeChecklist::saUploadUpdate:
        return Item->Local.Size;

      case TSynchronizeChecklist::saDownloadNew:
      case TSynchronizeChecklist::saDownloadUpdate:
        return Item->Remote.Size;

      default:
        DebugFail();
        return 0;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::FormShow(TObject * /*Sender*/)
{
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
  }

  FOrigListViewWindowProc(Message);
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
        ((ActionIndex == 0) || !IsItemSizeIrrelevant(Action)))
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
void __fastcall TSynchronizeChecklistDialog::StatusBarMouseMove(
  TObject * /*Sender*/, TShiftState /*Shift*/, int X, int /*Y*/)
{
  UnicodeString Hint;
  int IPanel = PanelAt(X);

  if (IPanel >= 0)
  {
    Hint = StatusBar->Panels->Items[IPanel]->Text;
    if (IPanel > 0)
    {
      Hint = FORMAT(L"%s\n%s", (Hint, FGeneralHint));
    }
  }

  if (Hint != StatusBar->Hint)
  {
    Application->CancelHint();
    StatusBar->Hint = Hint;
  }
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
        int ActionIndex = int(GetChecklistItemAction(ChecklistItem));
        int Diff = Item->Checked ? 1 : -1;

        FChecked[ActionIndex] += Diff;
        FChecked[0] += Diff;

        __int64 ItemSize = GetItemSize(ChecklistItem);
        if (!Item->Checked)
        {
          ItemSize = -ItemSize;
        }
        FCheckedSize[ActionIndex] += ItemSize;
        FCheckedSize[0] += ItemSize;

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
int __fastcall TSynchronizeChecklistDialog::CompareNumber(__int64 Value1,
  __int64 Value2)
{
  int Result;
  if (Value1 < Value2)
  {
    Result = -1;
  }
  else if (Value1 == Value2)
  {
    Result = 0;
  }
  else
  {
    Result = 1;
  }
  return Result;
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
      int ActionIndex = int(Action);

      FTotals[ActionIndex]--;
      if (Item->Checked)
      {
        FChecked[ActionIndex]--;

        __int64 ItemSize = GetItemSize(ChecklistItem);
        FCheckedSize[ActionIndex] -= ItemSize;
        FCheckedSize[0] -= ItemSize;
      }

      Action = NewAction;
      ActionIndex = int(Action);

      FTotals[ActionIndex]++;
      if (Item->Checked)
      {
        FChecked[ActionIndex]++;

        // item size may differ with action (0 for delete, but non-0 for new file transfer)
        __int64 ItemSize = GetItemSize(ChecklistItem);
        FCheckedSize[ActionIndex] += ItemSize;
        FCheckedSize[0] += ItemSize;
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
void __fastcall TSynchronizeChecklistDialog::Dispatch(void * Message)
{
  TMessage * M = reinterpret_cast<TMessage*>(Message);
  if (M->Msg == WM_SYSCOMMAND)
  {
    if (!HandleMinimizeSysCommand(*M))
    {
      TForm::Dispatch(Message);
    }
  }
  else
  {
    TForm::Dispatch(Message);
  }
}
//---------------------------------------------------------------------------
