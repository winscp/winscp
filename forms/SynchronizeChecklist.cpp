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
#include <CustomWinConfiguration.h>
//---------------------------------------------------------------------
#pragma link "IEListView"
#pragma link "NortonLikeListView"
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------
bool __fastcall DoSynchronizeChecklistDialog(TSynchronizeChecklist * Checklist,
  TSynchronizeMode Mode, int Params, const AnsiString LocalDirectory,
  const AnsiString RemoteDirectory)
{
  bool Result;
  TSynchronizeChecklistDialog * Dialog = new TSynchronizeChecklistDialog(
    Application, Mode, Params, LocalDirectory, RemoteDirectory);
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
  const AnsiString LocalDirectory, const AnsiString RemoteDirectory)
  : TForm(AOwner)
{
  FFormRestored = false;
  FMode = Mode;
  FParams = Params;
  FLocalDirectory = ExcludeTrailingBackslash(LocalDirectory);
  FRemoteDirectory = UnixExcludeTrailingBackslash(RemoteDirectory);
  UseSystemSettings(this);
  FChecklist = NULL;
  FChangingItem = NULL;
  FChangingItemIgnore = false;
  FChangingItemMass = false;
  FGeneralHint = StatusBar->Hint;

  FOrigListViewWindowProc = ListView->WindowProc;
  ListView->WindowProc = ListViewWindowProc;

  FSystemImageList = new TImageList(this);
  SHFILEINFO FileInfo;
  FSystemImageList->Handle = SHGetFileInfo(NULL, 0, &FileInfo, sizeof(FileInfo),
    SHGFI_SYSICONINDEX | SHGFI_SMALLICON);
  FSystemImageList->ShareImages = true;
  ListView->SmallImages = FSystemImageList;

  // header images mut be assigned after the small images, so it cannot
  // be done via DFM
  ListView->HeaderImages = ArrowImages;
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

  bool Result = (ShowModal() != mrCancel);

  if (Result)
  {
    for (int Index = 0; Index < ListView->Items->Count; Index++)
    {
      TListItem * Item = ListView->Items->Item[Index];
      // const violation !
      TSynchronizeChecklist::TItem * ChecklistItem =
        static_cast<TSynchronizeChecklist::TItem *>(Item->Data);
      ChecklistItem->Checked = Item->Checked;
    }

    TSynchronizeChecklistConfiguration FormConfiguration =
      CustomWinConfiguration->SynchronizeChecklist;
    FormConfiguration.ListParams = ListView->ColProperties->ParamsStr;

    AnsiString WindowParams = FormConfiguration.WindowParams;
    // if there is no main window, keep previous "custom pos" indication,
    bool CustomPos = (StrToIntDef(::CutToChar(WindowParams, ';', true), 0) != 0);
    if (Application->MainForm != NULL)
    {
      CustomPos = (Application->MainForm->BoundsRect != BoundsRect);
    }
    FormConfiguration.WindowParams =
      FORMAT("%d;%s", ((CustomPos ? 1 : 0), StoreForm(this)));

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
  TListItem * Item = ListView->Selected;
  while (Item != NULL)
  {
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
  EnableControl(CheckButton, !AllChecked);
  EnableControl(UncheckButton, !AllUnchecked);
  EnableControl(CheckAllButton, (FChecked[0] < FTotals[0]));
  EnableControl(UncheckAllButton, (FChecked[0] > 0));

  CheckItem->Enabled = CheckButton->Enabled;
  UncheckItem->Enabled = UncheckButton->Enabled;
  SelectAllItem->Enabled = (ListView->SelCount < ListView->Items->Count);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::CreateParams(TCreateParams & Params)
{
  if (!FFormRestored)
  {
    FFormRestored = True;
    AnsiString WindowParams = CustomWinConfiguration->SynchronizeChecklist.WindowParams;
    bool CustomPos = (StrToIntDef(::CutToChar(WindowParams, ';', true), 0) != 0);

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
void __fastcall TSynchronizeChecklistDialog::LoadItem(TListItem * Item)
{
  AnsiString S;
  const TSynchronizeChecklist::TItem * ChecklistItem = FChecklist->Item[Item->Index];
  Item->Data = const_cast<TSynchronizeChecklist::TItem *>(ChecklistItem);
  Item->Checked = ChecklistItem->Checked;
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

  if (ChecklistItem->Action == TSynchronizeChecklist::saDeleteRemote)
  {
    Item->SubItems->Add("");
    Item->SubItems->Add("");
    Item->SubItems->Add("");
  }
  else
  {
    S = ChecklistItem->Local.Directory;
    if (AnsiSameText(FLocalDirectory, S.SubString(1, FLocalDirectory.Length())))
    {
      S[1] = '.';
      S.Delete(2, FLocalDirectory.Length() - 1);
    }
    else
    {
      assert(false);
    }
    Item->SubItems->Add(S);
    if (ChecklistItem->Action == TSynchronizeChecklist::saDownloadNew)
    {
      Item->SubItems->Add("");
      Item->SubItems->Add("");
    }
    else
    {
      if (ChecklistItem->IsDirectory)
      {
        Item->SubItems->Add("");
      }
      else
      {
        Item->SubItems->Add(FormatFloat("#,##0", ChecklistItem->Local.Size));
      }
      Item->SubItems->Add(UserModificationStr(ChecklistItem->Local.Modification,
        ChecklistItem->Local.ModificationFmt));
    }
  }

  Item->SubItems->Add("");

  if (ChecklistItem->Action == TSynchronizeChecklist::saDeleteLocal)
  {
    Item->SubItems->Add("");
    Item->SubItems->Add("");
    Item->SubItems->Add("");
  }
  else
  {
    S = ChecklistItem->Remote.Directory;
    if (AnsiSameText(FRemoteDirectory, S.SubString(1, FRemoteDirectory.Length())))
    {
      S[1] = '.';
      S.Delete(2, FRemoteDirectory.Length() - 1);
    }
    else
    {
      assert(false);
    }
    Item->SubItems->Add(S);
    if (ChecklistItem->Action == TSynchronizeChecklist::saUploadNew)
    {
      Item->SubItems->Add("");
      Item->SubItems->Add("");
    }
    else
    {
      if (ChecklistItem->IsDirectory)
      {
        Item->SubItems->Add("");
      }
      else
      {
        Item->SubItems->Add(FormatFloat("#,##0", ChecklistItem->Remote.Size));
      }
      Item->SubItems->Add(UserModificationStr(ChecklistItem->Remote.Modification,
        ChecklistItem->Remote.ModificationFmt));
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::LoadList()
{
  memset(&FTotals, 0, sizeof(FTotals));
  memset(&FChecked, 0, sizeof(FChecked));
  FTotals[0] = FChecklist->Count;
  FChecked[0] = 0;

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
        LoadItem(ListView->Items->Add());
      }
      __finally
      {
        FChangingItemIgnore = false;
      }
      FTotals[int(ChecklistItem->Action)]++;
      if (ChecklistItem->Checked)
      {
        FChecked[int(ChecklistItem->Action)]++;
        FChecked[0]++;
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
  ListView->ColProperties->ParamsStr = CustomWinConfiguration->SynchronizeChecklist.ListParams;

  LoadList();
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::ListViewWindowProc(TMessage & Message)
{
  if (Message.Msg == CN_NOTIFY)
  {
    TWMNotify & NotifyMessage = reinterpret_cast<TWMNotify &>(Message);
    if (NotifyMessage.NMHdr->code == NM_CUSTOMDRAW)
    {
      // Due to a bug in VCL, OnAdvancedCustomDrawSubItem is not called for any
      // other stage except for cdPrePaint. So we must call it ourselves.
      TNMLVCustomDraw * CustomDraw =
        reinterpret_cast<TNMLVCustomDraw *>(NotifyMessage.NMHdr);
      if (FLAGSET(CustomDraw->nmcd.dwDrawStage, CDDS_ITEM) &&
          FLAGSET(CustomDraw->nmcd.dwDrawStage, CDDS_SUBITEM) &&
          FLAGSET(CustomDraw->nmcd.dwDrawStage, CDDS_ITEMPOSTPAINT))
      {
        TListItem * Item = ListView->Items->Item[CustomDraw->nmcd.dwItemSpec];
        bool DefaultDraw = true; // not used
        // do not know how to convert CustomDraw->nmcd.uItemState to TCustomDrawState
        ListViewAdvancedCustomDrawSubItem(ListView, Item, CustomDraw->iSubItem,
          TCustomDrawState(), cdPostPaint, DefaultDraw);
      }
    }
  }
  FOrigListViewWindowProc(Message);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::ListViewAdvancedCustomDrawSubItem(
  TCustomListView * /*Sender*/, TListItem * Item, int SubItem,
  TCustomDrawState /*State*/, TCustomDrawStage Stage, bool & /*DefaultDraw*/)
{
  if ((SubItem == 4) && (Stage == cdPostPaint))
  {
    if (ActionImages->Width <= ListView->Columns->Items[SubItem]->Width)
    {
      const TSynchronizeChecklist::TItem * ChecklistItem =
        static_cast<const TSynchronizeChecklist::TItem *>(Item->Data);
      TRect R = Item->DisplayRect(drBounds);
      for (int Index = 0; Index < SubItem; Index++)
      {
        R.Left += ListView->Columns->Items[Index]->Width;
      }
      R.Left +=
        (ListView->Columns->Items[SubItem]->Width - ActionImages->Width) / 2;
      ImageList_Draw(reinterpret_cast<HIMAGELIST>(ActionImages->Handle),
        int(ChecklistItem->Action), ListView->Canvas->Handle,
        R.Left, R.Top, ILD_TRANSPARENT);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::StatusBarDrawPanel(
  TStatusBar * StatusBar, TStatusPanel * Panel, const TRect & Rect)
{
  bool Possible;
  TSynchronizeChecklist::TAction Action = TSynchronizeChecklist::TAction(Panel->Index);
  switch (Action)
  {
    case TSynchronizeChecklist::saNone:
      Possible = true;
      break;

    case TSynchronizeChecklist::saUploadNew:
      Possible = ((FMode == smRemote) || (FMode == smBoth)) &&
        FLAGCLEAR(FParams, spTimestamp);
      break;

    case TSynchronizeChecklist::saDownloadNew:
      Possible = ((FMode == smLocal) || (FMode == smBoth)) &&
        FLAGCLEAR(FParams, spTimestamp);
      break;

    case TSynchronizeChecklist::saUploadUpdate:
      Possible = ((FMode == smRemote) || (FMode == smBoth));
      break;

    case TSynchronizeChecklist::saDownloadUpdate:
      Possible = ((FMode == smLocal) || (FMode == smBoth));
      break;

    case TSynchronizeChecklist::saDeleteRemote:
      Possible = (FMode == smRemote) &&
        FLAGCLEAR(FParams, spTimestamp);
      break;

    case TSynchronizeChecklist::saDeleteLocal:
      Possible = (FMode == smLocal) &&
        FLAGCLEAR(FParams, spTimestamp);
      break;

    default:
      assert(false);
      Possible = false;
      break;
  }

  int ImageIndex = Panel->Index;
  AnsiString PanelText;
  if (Possible)
  {
    PanelText = FORMAT(LoadStrPart(SYNCHRONIZE_SELECTED_ACTIONS, 1),
      (FormatFloat("#,##0", FChecked[Panel->Index]),
       FormatFloat("#,##0", FTotals[Panel->Index])));
  }
  else
  {
    PanelText = LoadStrPart(SYNCHRONIZE_SELECTED_ACTIONS, 2);
  }

  StatusBar->Canvas->TextRect(Rect, Rect.left + 18, Rect.top + 1, PanelText);
  ActionImages->Draw(StatusBar->Canvas,
    Rect.Left + 1, Rect.Top, ImageIndex, Possible);
}
//---------------------------------------------------------------------------
int __fastcall TSynchronizeChecklistDialog::PanelAt(int X)
{
  int Result = 0;
  while ((X > StatusBar->Panels->Items[Result]->Width) &&
    (Result < StatusBar->Panels->Count - 1))
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
  AnsiString Hint;
  int IPanel = PanelAt(X);

  if (IPanel >= 0)
  {
    Hint = StatusBar->Panels->Items[IPanel]->Text;
    if (IPanel > 0)
    {
      Hint = FORMAT("%s\n%s", (Hint, FGeneralHint));
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
      assert(Item->Data != NULL);
      if ((FChangingItemChecked != Item->Checked) && (Item->Data != NULL))
      {
        const TSynchronizeChecklist::TItem * ChecklistItem =
          static_cast<const TSynchronizeChecklist::TItem *>(Item->Data);
        if (Item->Checked)
        {
          FChecked[int(ChecklistItem->Action)]++;
          FChecked[0]++;
        }
        else
        {
          FChecked[int(ChecklistItem->Action)]--;
          FChecked[0]--;
        }

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
    assert(FChangingItem == NULL);
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
void __fastcall TSynchronizeChecklistDialog::CheckAllButtonClick(TObject * Sender)
{
  CheckAll(Sender == CheckAllButton);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::CheckButtonClick(
  TObject * Sender)
{
  FChangingItemMass = true;
  try
  {
    TListItem * Item = ListView->Selected;
    while (Item != NULL)
    {
      TComponent * Component = dynamic_cast<TComponent *>(Sender);
      assert(Component != NULL);
      Item->Checked = (Component->Tag != 0);
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
void __fastcall TSynchronizeChecklistDialog::ListViewSelectItem(
  TObject * /*Sender*/, TListItem * /*Item*/, bool /*Selected*/)
{
  // Delayed update of button status in case many items are being selected at once
  // Also change of selection causes buttons to flash, as for short period of time,
  // no item is selected
  UpdateTimer->Enabled = true;
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklistDialog::StatusBarResize(
  TObject * Sender)
{
  RepaintStatusBar(dynamic_cast<TCustomStatusBar *>(Sender));
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
      const TSynchronizeChecklist::TItem * ChecklistItem =
        static_cast<const TSynchronizeChecklist::TItem *>(Item->Data);
      bool WantedAction = (int(ChecklistItem->Action) == Action);
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
void __fastcall TSynchronizeChecklistDialog::SelectAllItemClick(
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
  const TSynchronizeChecklist::TItem * ChecklistItem1 =
    static_cast<const TSynchronizeChecklist::TItem *>(Item1->Data);
  const TSynchronizeChecklist::TItem * ChecklistItem2 =
    static_cast<const TSynchronizeChecklist::TItem *>(Item2->Data);

  TIEListViewColProperties * ColProperties =
    dynamic_cast<TIEListViewColProperties *>(ListView->ColProperties);

  switch (ColProperties->SortColumn)
  {
    case 0: // name
      Compare = CompareText(ChecklistItem1->GetFileName(), ChecklistItem2->GetFileName());
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

    case 4: // action
      Compare = CompareNumber(ChecklistItem1->Action, ChecklistItem2->Action);
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
      Compare = CompareText(ChecklistItem1->Local.Directory, ChecklistItem2->Local.Directory);
    }
    else
    {
      assert(!ChecklistItem1->Remote.Directory.IsEmpty());
      Compare = CompareText(ChecklistItem1->Remote.Directory, ChecklistItem2->Remote.Directory);
    }

    if (Compare == 0)
    {
      Compare = CompareText(ChecklistItem1->GetFileName(), ChecklistItem2->GetFileName());
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
