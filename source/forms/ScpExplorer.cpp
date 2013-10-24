//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "ScpExplorer.h"

#include <Common.h>
#include <CoreMain.h>

#include "NonVisual.h"
#include "Glyphs.h"
#include "Tools.h"
#include "WinConfiguration.h"
#include <VCLCommon.h>
#include <TextsWin.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "CustomDirView"
#pragma link "CustomScpExplorer"
#pragma link "CustomUnixDirView"
#pragma link "IEListView"
#pragma link "NortonLikeListView"
#pragma link "UnixDirView"
#pragma link "IEComboBox"
#pragma link "CustomDriveView"
#pragma link "UnixDriveView"
#pragma link "TB2Dock"
#pragma link "TBX"
#pragma link "TB2Item"
#pragma link "TB2Toolbar"
#pragma link "TBXStatusBars"
#pragma link "TBXExtItems"
#pragma link "TB2ExtItems"
#pragma link "TBXToolPals"
#pragma link "TBXLists"
#pragma link "ThemePageControl"
#pragma link "PathLabel"
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------------
__fastcall TScpExplorerForm::TScpExplorerForm(TComponent* Owner)
        : TCustomScpExplorerForm(Owner)
{
  UnixPathComboBox->Images = FSystemImageList;
  UnixPathComboBox->SubMenuImages = UnixPathComboBox->Images;

  BackButton->LinkSubitems = HistoryMenu(osRemote, true)->Items;
  ForwardButton->LinkSubitems = HistoryMenu(osRemote, false)->Items;

  TopDock->PopupMenu = NonVisualDataModule->ExplorerBarPopup;
  RemoteStatusBar->PopupMenu = TopDock->PopupMenu;
  QueueDock->PopupMenu = TopDock->PopupMenu;
  QueueLabel->PopupMenu = TopDock->PopupMenu;
  RemoteDriveView->PopupMenu = TopDock->PopupMenu;
  BottomDock->PopupMenu = TopDock->PopupMenu;
  LeftDock->PopupMenu = TopDock->PopupMenu;
  RightDock->PopupMenu = TopDock->PopupMenu;
  reinterpret_cast<TLabel*>(RemotePanelSplitter)->OnDblClick = RemotePanelSplitterDblClick;

  QueuePanel->Parent = RemotePanel;
  QueueSplitter->Parent = RemotePanel;

  // set common explorer shorcuts to our actions
  NonVisualDataModule->ExplorerShortcuts();

  NonVisualDataModule->QueueSpeedComboBoxItem(QueueSpeedComboBoxItem);
}
//---------------------------------------------------------------------------
void __fastcall TScpExplorerForm::RestoreFormParams()
{
  assert(Configuration);

  TCustomScpExplorerForm::RestoreFormParams();
  RestoreForm(WinConfiguration->ScpExplorer.WindowParams, this);
}
//---------------------------------------------------------------------------
void __fastcall TScpExplorerForm::ConfigurationChanged()
{
  TCustomScpExplorerForm::ConfigurationChanged();
  UpdateRemotePathComboBox(UnixPathComboBox, true);
}
//---------------------------------------------------------------------------
void __fastcall TScpExplorerForm::RestoreParams()
{
  assert(Configuration);

  TCustomScpExplorerForm::RestoreParams();

  bool HadHandleAllocated = RemoteDirView->HandleAllocated();
  RemoteDirView->UnixColProperties->ParamsStr = WinConfiguration->ScpExplorer.DirViewParams;
  RemoteDirView->UnixColProperties->ExtVisible = false; // just to make sure
  RemoteDirView->ViewStyle = (TViewStyle)WinConfiguration->ScpExplorer.ViewStyle;
  if (HadHandleAllocated)
  {
    // This is here to make our persistence checks in VerifyControl pass,
    // but we do not want the view linger in the middle of delayed recreation anyway
    RemoteDirView->HandleNeeded();
  }

  LoadToolbarsLayoutStr(WinConfiguration->ScpExplorer.ToolbarsLayout);
  SessionsPageControl->Visible = WinConfiguration->ScpExplorer.SessionsTabs;
  RemoteStatusBar->Visible = WinConfiguration->ScpExplorer.StatusBar;
  RemoteDriveView->Visible = WinConfiguration->ScpExplorer.DriveView;
  RemoteDriveView->Width =
    LoadDimension(WinConfiguration->ScpExplorer.DriveViewWidth, WinConfiguration->ScpExplorer.DriveViewWidthPixelsPerInch);
}
//---------------------------------------------------------------------------
void __fastcall TScpExplorerForm::StoreParams()
{
  assert(Configuration);

  Configuration->BeginUpdate();
  try
  {
    WinConfiguration->ScpExplorer.ToolbarsLayout = GetToolbarsLayoutStr();
    WinConfiguration->ScpExplorer.SessionsTabs = SessionsPageControl->Visible;
    WinConfiguration->ScpExplorer.StatusBar = RemoteStatusBar->Visible;

    WinConfiguration->ScpExplorer.WindowParams = StoreForm(this);
    WinConfiguration->ScpExplorer.DirViewParams = RemoteDirView->UnixColProperties->ParamsStr;
    WinConfiguration->ScpExplorer.ViewStyle = RemoteDirView->ViewStyle;
    WinConfiguration->ScpExplorer.DriveView = RemoteDriveView->Visible;
    WinConfiguration->ScpExplorer.DriveViewWidth = RemoteDriveView->Width;
    WinConfiguration->ScpExplorer.DriveViewWidthPixelsPerInch = Screen->PixelsPerInch;
    TCustomScpExplorerForm::StoreParams();
  }
  __finally
  {
    WinConfiguration->EndUpdate();
  }
}
//---------------------------------------------------------------------------
bool __fastcall TScpExplorerForm::CopyParamDialog(TTransferDirection Direction,
  TTransferType Type, Boolean Temp, TStrings * FileList,
  UnicodeString & TargetDirectory, TGUICopyParamType & CopyParam, bool Confirm,
  bool DragDrop)
{
  // Temp means d&d here so far, may change in future!
  if ((Direction == tdToLocal) && !Temp && TargetDirectory.IsEmpty())
  {
    TargetDirectory = WinConfiguration->ScpExplorer.LastLocalTargetDirectory;
  }
  bool Result = TCustomScpExplorerForm::CopyParamDialog(
    Direction, Type, Temp, FileList, TargetDirectory, CopyParam, Confirm, DragDrop);
  if (Result && (Direction == tdToLocal) && !Temp)
  {
    WinConfiguration->ScpExplorer.LastLocalTargetDirectory = TargetDirectory;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TScpExplorerForm::DoShow()
{
  TCustomScpExplorerForm::DoShow();

  ActiveControl = RemoteDirView;
}
//---------------------------------------------------------------------------
bool __fastcall TScpExplorerForm::AllowedAction(TAction * Action, TActionAllowed Allowed)
{
  if (Allowed == aaUpdate)
  {
    if (Action == NonVisualDataModule->FileListToCommandLineAction)
    {
      Action->Visible = false;
    }
  }
  #define FLAG ((TActionFlag)(Action->Tag))
  return
    // always require Explorer flag
    (FLAG & afExplorer) &&
    // if action is execution or update, we don't require any other flag
    // if we check for shortcut, we require that action is designed for remote panel
    ((Allowed != aaShortCut) || (FLAG & afRemote));
  #undef FLAG
}
//---------------------------------------------------------------------------
TControl * __fastcall TScpExplorerForm::GetComponent(Byte Component)
{
  switch (Component) {
    case fcSessionToolbar: return SessionToolbar;
    case fcCustomCommandsBand: return CustomCommandsToolbar;
    case fcColorMenu: return reinterpret_cast<TControl*>(ColorMenuItem);
    case fcTransferDropDown: return reinterpret_cast<TControl*>(TransferDropDown);
    case fcTransferList: return reinterpret_cast<TControl*>(TransferList);
    case fcTransferLabel: return reinterpret_cast<TControl*>(TransferLabel);

    case fcExplorerMenuBand: return MenuToolbar;
    case fcExplorerAddressBand: return AddressToolbar;
    case fcExplorerToolbarBand: return ButtonsToolbar;
    case fcExplorerSelectionBand: return SelectionToolbar;
    case fcExplorerSessionBand: return SessionToolbar;
    case fcExplorerPreferencesBand: return PreferencesToolbar;
    case fcExplorerSortBand: return SortToolbar;
    case fcExplorerUpdatesBand: return UpdatesToolbar;
    case fcExplorerTransferBand: return TransferToolbar;
    case fcExplorerCustomCommandsBand: return CustomCommandsToolbar;
    default: return TCustomScpExplorerForm::GetComponent(Component);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpExplorerForm::SynchronizeDirectories()
{
  UnicodeString LocalDirectory = WinConfiguration->ScpExplorer.LastLocalTargetDirectory;
  UnicodeString RemoteDirectory = RemoteDirView->PathName;
  if (DoSynchronizeDirectories(LocalDirectory, RemoteDirectory, false))
  {
    WinConfiguration->ScpExplorer.LastLocalTargetDirectory = LocalDirectory;
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpExplorerForm::FullSynchronizeDirectories()
{
  UnicodeString LocalDirectory = WinConfiguration->ScpExplorer.LastLocalTargetDirectory;
  UnicodeString RemoteDirectory = RemoteDirView->PathName;
  bool SaveMode = true;
  TSynchronizeMode Mode = (TSynchronizeMode)GUIConfiguration->SynchronizeMode;
  if (DoFullSynchronizeDirectories(LocalDirectory, RemoteDirectory, Mode,
        SaveMode, false))
  {
    WinConfiguration->ScpExplorer.LastLocalTargetDirectory = LocalDirectory;
    if (SaveMode)
    {
      GUIConfiguration->SynchronizeMode = Mode;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpExplorerForm::FixControlsPlacement()
{
  TCustomScpExplorerForm::FixControlsPlacement();

  TControl * ControlsOrder[] =
    { RemoteDirView, QueueSplitter, QueuePanel, BottomDock, RemoteStatusBar };
  SetVerticalControlsOrder(ControlsOrder, LENOF(ControlsOrder));

  TControl * RemoteControlsOrder[] =
    { RemoteDriveView, RemotePanelSplitter, RemoteDirView };
  SetHorizontalControlsOrder(RemoteControlsOrder, LENOF(RemoteControlsOrder));
}
//---------------------------------------------------------------------------
void __fastcall TScpExplorerForm::RemoteDirViewUpdateStatusBar(
  TObject * /*Sender*/, const TStatusFileInfo & FileInfo)
{
  FStatusBarFileText = FileStatusBarText(FileInfo);
  if (!CancelNote(false))
  {
    // if there's no note to cancel, we need to update status bar explicitly
    UpdateStatusBar();
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpExplorerForm::RemotePanelSplitterDblClick(TObject * /*Sender*/)
{
  // for some reason PostComponentHide is not necessary here (see queue panel)
  ComponentVisible[fcRemoteTree] = false;
}
//---------------------------------------------------------------------------
void __fastcall TScpExplorerForm::UpdateStatusPanelText(TTBXStatusPanel * Panel)
{
  Panel->Caption = FStatusBarFileText;
}
//---------------------------------------------------------------------------
void __fastcall TScpExplorerForm::UnixPathComboBoxBeginEdit(
  TTBEditItem * /*Sender*/, TTBEditItemViewer * /*Viewer*/, TEdit * EditControl)
{
  InstallPathWordBreakProc(EditControl);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TScpExplorerForm::RemotePathComboBoxText()
{
  UnicodeString Result;

  if (WinConfiguration->ScpExplorer.ShowFullAddress)
  {
    Result = UnixExcludeTrailingBackslash(RemoteDirView->Path);
  }
  else
  {
    // this is called couple of times before the combo box is populated
    if (UnixPathComboBox->Strings->Count > 0)
    {
      Result = UnixPathComboBox->Strings->Strings[UnixPathComboBox->Strings->Count - 1];
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TScpExplorerForm::UnixPathComboBoxAcceptText(
  TObject * /*Sender*/, UnicodeString & NewText, bool & /*Accept*/)
{
  if (RemoteDirView->Path != NewText)
  {
    RemoteDirView->Path = NewText;
    NewText = RemotePathComboBoxText();
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpExplorerForm::UpdateRemotePathComboBox(
  TTBXComboBoxItem * RemotePathComboBox, bool TextOnly)
{
  TCustomScpExplorerForm::UpdateRemotePathComboBox(RemotePathComboBox, TextOnly);

  UnixPathComboBox->Text = RemotePathComboBoxText();
}
//---------------------------------------------------------------------------
void __fastcall TScpExplorerForm::RemoteDirViewPathChange(
  TCustomDirView * /*Sender*/)
{
  UpdateRemotePathComboBox(UnixPathComboBox, false);
}
//---------------------------------------------------------------------------
void __fastcall TScpExplorerForm::ToolbarItemResize(TTBXCustomDropDownItem * Item, int Width)
{
  TCustomScpExplorerForm::ToolbarItemResize(Item, Width);
  if (Item == UnixPathComboBox)
  {
    dynamic_cast<TTBXComboBoxItem *>(Item)->MinListWidth = Width - 4;
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpExplorerForm::QueueSubmenuItemPopup(
  TTBCustomItem * /*Sender*/, bool /*FromLink*/)
{
  NonVisualDataModule->QueueSpeedComboBoxItemUpdate(QueueSpeedComboBoxItem);
}
//---------------------------------------------------------------------------
void __fastcall TScpExplorerForm::ChangePath(TOperationSide /*Side*/)
{
  assert(false);
}
//---------------------------------------------------------------------------
bool __fastcall TScpExplorerForm::UpdateToolbarDisplayMode()
{
  bool Result = TCustomScpExplorerForm::UpdateToolbarDisplayMode();
  if (Result)
  {
    // address combo width needs to be updated as caption visibility has changed
    ToolBarResize(AddressToolbar);
  }
  return Result;
}
//---------------------------------------------------------------------------
