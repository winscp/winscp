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
#include <WinConfiguration.h>
//---------------------------------------------------------------------
#pragma link "IEListView"
#pragma link "NortonLikeListView"
#pragma link "PngImageList"
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
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

  FOrigListViewWindowProc = ListView->WindowProc;
  ListView->WindowProc = ListViewWindowProc;

  FSystemImageList = SharedSystemImageList(false);
  ListView->SmallImages = FSystemImageList;

  // header images mut be assigned after the small images, so it cannot
  // be done via DFM
  ListView->HeaderImages = ArrowImages;

  CustomCommandsButton->Visible = (FOnCustomCommandMenu != NULL);
  MenuButton(CustomCommandsButton);
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
      // const violation !
      TSynchronizeChecklist::TItem * ChecklistItem =
        static_cast<TSynchronizeChecklist::TItem *>(Item->Data);
      ChecklistItem->Checked = Item->Checked;
    }

    TSynchronizeChecklistConfiguration FormConfiguration =
      CustomWinConfiguration->SynchronizeChecklist;
    FormConfiguration.ListParams = ListView->ColProperties->ParamsStr;

    UnicodeString WindowParams = FormConfiguration.WindowParams;
    // if there is no main window, keep previous "custom pos" indication,
    bool CustomPos = (StrToIntDef(::CutToChar(WindowParams, L';', true), 0) != 0);
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
    const TSynchronizeChecklist::TItem * ChecklistItem =
      static_cast<const TSynchronizeChecklist::TItem *>(Item->Data);
    if ((ChecklistItem->Action == TSynchronizeChecklist::saUploadUpdate) ||
        (ChecklistItem->Action == TSynchronizeChecklist::saDownloadUpdate))
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
  EnableControl(CheckButton, !AllChecked);
  EnableControl(UncheckButton, !AllUnchecked);
  EnableControl(CheckAllButton, (FChecked[0] < FTotals[0]));
  EnableControl(UncheckAllButton, (FChecked[0] > 0));
  EnableControl(UncheckAllButton, (FChecked[0] > 0));
  EnableControl(CustomCommandsButton, AnyBoth && !AnyNonBoth);

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
    UnicodeString WindowParams = CustomWinConfiguration->SynchronizeChecklist.WindowParams;
    bool CustomPos = (StrToIntDef(::CutToChar(WindowParams, L';', true), 0) != 0);

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
  UnicodeString S;
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
    Item->SubItems->Add(L"");
    Item->SubItems->Add(L"");
    Item->SubItems->Add(L"");
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
      FAIL;
    }
    Item->SubItems->Add(S);
    if (ChecklistItem->Action == TSynchronizeChecklist::saDownloadNew)
    {
      Item->SubItems->Add(L"");
      Item->SubItems->Add(L"");
    }
    else
    {
      if (ChecklistItem->IsDirectory)
      {
        Item->SubItems->Add(L"");
      }
      else
      {
        Item->SubItems->Add(
          FormatPanelBytes(ChecklistItem->Local.Size, WinConfiguration->FormatSizeBytes));
      }
      Item->SubItems->Add(UserModificationStr(ChecklistItem->Local.Modification,
        ChecklistItem->Local.ModificationFmt));
    }
  }

  Item->SubItems->Add(L"");

  if (ChecklistItem->Action == TSynchronizeChecklist::saDeleteLocal)
  {
    Item->SubItems->Add(L"");
    Item->SubItems->Add(L"");
    Item->SubItems->Add(L"");
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
      FAIL;
    }
    Item->SubItems->Add(S);
    if (ChecklistItem->Action == TSynchronizeChecklist::saUploadNew)
    {
      Item->SubItems->Add(L"");
      Item->SubItems->Add(L"");
    }
    else
    {
      if (ChecklistItem->IsDirectory)
      {
        Item->SubItems->Add(L"");
      }
      else
      {
        Item->SubItems->Add(
          FormatPanelBytes(ChecklistItem->Remote.Size, WinConfiguration->FormatSizeBytes));
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
          FLAGSET(CustomDraw->nmcd.dwDrawStage, CDDS_ITEMPOSTPAINT) &&
          (CustomDraw->iSubItem == 4) &&
          (ActionImages->Width <= ListView->Columns->Items[CustomDraw->iSubItem]->Width))
      {
        TListItem * Item = ListView->Items->Item[CustomDraw->nmcd.dwItemSpec];
        const TSynchronizeChecklist::TItem * ChecklistItem =
          static_cast<const TSynchronizeChecklist::TItem *>(Item->Data);
        TRect R = Item->DisplayRect(drBounds);
        for (int Index = 0; Index < CustomDraw->iSubItem; Index++)
        {
          R.Left += ListView->Columns->Items[Index]->Width;
        }
        R.Left +=
          (ListView->Columns->Items[CustomDraw->iSubItem]->Width - ActionImages->Width) / 2;
        // doing this from ListViewAdvancedCustomDraw corrupts list view on Windows 7
        ImageList_Draw(reinterpret_cast<HIMAGELIST>(ActionImages->Handle),
          int(ChecklistItem->Action), CustomDraw->nmcd.hdc,
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
      Possible =
        ((FMode == smRemote) || (FMode == smBoth)) &&
        (FLAGCLEAR(FParams, spNotByTime) || FLAGSET(FParams, spBySize));
      break;

    case TSynchronizeChecklist::saDownloadUpdate:
      Possible =
        ((FMode == smLocal) || (FMode == smBoth)) &&
        (FLAGCLEAR(FParams, spNotByTime) || FLAGSET(FParams, spBySize));
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
      FAIL;
      Possible = false;
      break;
  }

  int ImageIndex = Panel->Index;
  UnicodeString PanelText;
  if (Possible)
  {
    PanelText = FORMAT(LoadStrPart(SYNCHRONIZE_SELECTED_ACTIONS, 1),
      (FormatNumber(FChecked[Panel->Index]),
       FormatNumber(FTotals[Panel->Index])));
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
  ActionImages->Draw(StatusBar->Canvas, X, Y, ImageIndex, Possible);
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
      Compare = AnsiCompareText(ChecklistItem1->Local.Directory, ChecklistItem2->Local.Directory);
    }
    else
    {
      assert(!ChecklistItem1->Remote.Directory.IsEmpty());
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
void __fastcall TSynchronizeChecklistDialog::CustomCommandsButtonClick(
  TObject * /*Sender*/)
{
  TStrings * LocalFileList = new TStringList();
  TStrings * RemoteFileList = new TStringList();
  try
  {
    TListItem * Item = ListView->Selected;
    assert(Item != NULL);

    while (Item != NULL)
    {
      const TSynchronizeChecklist::TItem * ChecklistItem =
        static_cast<const TSynchronizeChecklist::TItem *>(Item->Data);

      assert((ChecklistItem->Action == TSynchronizeChecklist::saUploadUpdate) ||
             (ChecklistItem->Action == TSynchronizeChecklist::saDownloadUpdate));
      assert(ChecklistItem->RemoteFile != NULL);

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

  assert(FOnCustomCommandMenu != NULL);
  FOnCustomCommandMenu(CustomCommandsButton,
    CalculatePopupRect(CustomCommandsButton), LocalFileList, RemoteFileList);
}
//---------------------------------------------------------------------------
