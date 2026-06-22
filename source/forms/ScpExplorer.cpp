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
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
__fastcall TScpExplorerForm::TScpExplorerForm(TComponent* Owner)
        : TCustomScpExplorerForm(Owner)
{
  BackButton->LinkSubitems = HistoryMenu(osRemote, true)->Items;
  ForwardButton->LinkSubitems = HistoryMenu(osRemote, false)->Items;

  TopDock->PopupMenu = NonVisualDataModule->ExplorerBarPopup;
  CopyPopup(RemoteStatusBar, TopDock);
  CopyPopup(QueueDock, TopDock);
  CopyPopup(QueueLabel, TopDock);
  CopyPopup(RemoteDriveView, TopDock);
  CopyPopup(BottomDock, TopDock);
  CopyPopup(LeftDock, TopDock);
  CopyPopup(RightDock, TopDock);
  CopyPopup(QueueFileList, TopDock);
  CopyPopup(QueueFileListSplitter, TopDock);
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
  DebugAssert(Configuration);

  TCustomScpExplorerForm::RestoreFormParams();
  RestoreForm(WinConfiguration->ScpExplorer.WindowParams, this, false, ScpExplorerWindowParamsDefault);
}
//---------------------------------------------------------------------------
void __fastcall TScpExplorerForm::ConfigurationChanged()
{
  TCustomScpExplorerForm::ConfigurationChanged();
  UpdateRemotePathComboBox(true);
}
//---------------------------------------------------------------------------
void __fastcall TScpExplorerForm::RestoreParams()
{
  DebugAssert(Configuration);

  TCustomScpExplorerForm::RestoreParams();

  bool HadHandleAllocated = RemoteDirView->HandleAllocated();
  RemoteDirView->UnixColProperties->ParamsStr = WinConfiguration->ScpExplorer.DirViewParams;
  RemoteDirView->UnixColProperties->ExtVisible = false; // just to make sure
  RemoteDirView->DirViewStyle = (TDirViewStyle)WinConfiguration->ScpExplorer.ViewStyle;
  if (HadHandleAllocated)
  {
    // This is here to make our persistence checks in VerifyControl pass,
    // but we do not want the view linger in the middle of delayed recreation anyway
    RemoteDirView->HandleNeeded();
  }

  LoadToolbarsLayoutStr(WinConfiguration->ScpExplorer.ToolbarsLayout, WinConfiguration->ScpExplorer.ToolbarsButtons);
  if (IsUWP())
  {
    UpdatesToolbar->Visible = false;
  }
  SessionsPageControl->Visible = WinConfiguration->ScpExplorer.SessionsTabs;
  RemoteStatusBar->Visible = WinConfiguration->ScpExplorer.StatusBar;
  RemoteDrivePanel->Visible = WinConfiguration->ScpExplorer.DriveView;
  RemoteDrivePanel->Width =
    LoadDimension(
      WinConfiguration->ScpExplorer.DriveViewWidth, WinConfiguration->ScpExplorer.DriveViewWidthPixelsPerInch, this);
}
//---------------------------------------------------------------------------
void __fastcall TScpExplorerForm::StoreParams()
{
  DebugAssert(Configuration);

  Configuration->BeginUpdate();
  try
  {
    WinConfiguration->ScpExplorer.ToolbarsLayout = GetToolbarsLayoutStr();
    WinConfiguration->ScpExplorer.ToolbarsButtons = GetToolbarsButtonsStr();
    WinConfiguration->ScpExplorer.SessionsTabs = SessionsPageControl->Visible;
    WinConfiguration->ScpExplorer.StatusBar = RemoteStatusBar->Visible;

    WinConfiguration->ScpExplorer.WindowParams = StoreForm(this);
    WinConfiguration->ScpExplorer.DirViewParams = RemoteDirView->UnixColProperties->ParamsStr;
    WinConfiguration->ScpExplorer.ViewStyle = RemoteDirView->DirViewStyle;
    WinConfiguration->ScpExplorer.DriveView = RemoteDrivePanel->Visible;
    WinConfiguration->ScpExplorer.DriveViewWidth = RemoteDrivePanel->Width;
    WinConfiguration->ScpExplorer.DriveViewWidthPixelsPerInch = GetControlPixelsPerInch(this);
    TCustomScpExplorerForm::StoreParams();
  }
  __finally
  {
    WinConfiguration->EndUpdate();
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TScpExplorerForm::DefaultDownloadTargetDirectory()
{
  UnicodeString Result = WinConfiguration->ScpExplorer.LastLocalTargetDirectory;
  if (!DirectoryExists(Result))
  {
    Result = GetPersonalFolder();
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TScpExplorerForm::CopyParamDialogAfter(
  TTransferDirection Direction, bool Temp, const UnicodeString & TargetDirectory)
{
  TCustomScpExplorerForm::CopyParamDialogAfter(Direction, Temp, TargetDirectory);
  if ((Direction == tdToLocal) && !Temp)
  {
    WinConfiguration->ScpExplorer.LastLocalTargetDirectory = TargetDirectory;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TScpExplorerForm::CopyParamDialog(TTransferDirection Direction,
  TTransferType Type, Boolean Temp, TStrings * FileList,
  UnicodeString & TargetDirectory, TGUICopyParamType & CopyParam, bool Confirm,
  bool DragDrop, int Options)
{
  // Temp means d&d here so far, may change in future!
  if ((Direction == tdToLocal) && !Temp && TargetDirectory.IsEmpty())
  {
    TargetDirectory = DefaultDownloadTargetDirectory();
  }
  bool Result = TCustomScpExplorerForm::CopyParamDialog(
    Direction, Type, Temp, FileList, TargetDirectory, CopyParam, Confirm,
    DragDrop, Options);
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TScpExplorerForm::DoShow()
{
  AddStartupSequence(L"W");
  // See comment in TScpCommanderForm::DoShow()
  UpdateControls();

  if (DirView(osOther)->Enabled)
  {
    DirView(osOther)->SetFocus();
  }

  AddStartupSequence(L"U");
  TCustomScpExplorerForm::DoShow();
  AddStartupSequence(L"O");
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
    TCustomScpExplorerForm::AllowedAction(Action, Allowed) &&
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
    case fcSessionToolbar: return SessionToolbar2;
    case fcCustomCommandsBand: return CustomCommandsToolbar;
    case fcColorMenu: return reinterpret_cast<TControl*>(ColorMenuItem);
    case fcTransferDropDown: return reinterpret_cast<TControl*>(TransferDropDown);
    case fcTransferList: return reinterpret_cast<TControl*>(TransferList);
    case fcTransferLabel: return reinterpret_cast<TControl*>(TransferLabel);
    case fcRemotePathComboBox: return reinterpret_cast<TControl*>(UnixPathComboBox);
    case fcMenu: return MenuToolbar;

    case fcExplorerMenuBand: return MenuToolbar;
    case fcExplorerAddressBand: return AddressToolbar;
    case fcExplorerToolbarBand: return ButtonsToolbar;
    case fcExplorerSelectionBand: return SelectionToolbar;
    case fcExplorerSessionBand: return SessionToolbar2;
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
  if (DoSynchronizeDirectories(LocalDirectory, RemoteDirectory, -1))
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
  int Params = GUIConfiguration->SynchronizeParams;
  if (DoFullSynchronizeDirectories(LocalDirectory, RemoteDirectory, Mode, Params, SaveMode, -1) >= 0)
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
    { RemoteDirPanel, QueueSplitter, QueuePanel, BottomDock, RemoteStatusBar };
  SetVerticalControlsOrder(ControlsOrder, LENOF(ControlsOrder));

  TControl * RemoteControlsOrder[] =
    { RemoteDrivePanel, RemotePanelSplitter, RemoteDirPanel };
  SetHorizontalControlsOrder(RemoteControlsOrder, LENOF(RemoteControlsOrder));
}
//---------------------------------------------------------------------------
void __fastcall TScpExplorerForm::RemoteDirViewUpdateStatusBar(
  TObject * /*Sender*/, const TStatusFileInfo & FileInfo)
{
  FStatusBarFileText = FileStatusBarText(FileInfo, osRemote);
  if (!CancelNote(false))
  {
    // if there's no note to cancel, we need to update status bar explicitly
    UpdateStatusBar();
  }
  UpdateFileStatusExtendedPanels(RemoteStatusBar, FileInfo);
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
  if (!FFailedAddress.IsEmpty())
  {
    EditControl->Text = FFailedAddress;
    EditControl->SelectAll();
  }
  FFailedAddress = UnicodeString();
}
//---------------------------------------------------------------------------
void __fastcall TScpExplorerForm::AddressToolbarEndModal(TObject * /*Sender*/)
{
  if (!FFailedAddress.IsEmpty())
  {
    GoToAddress();
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TScpExplorerForm::RemotePathComboBoxText()
{
  UnicodeString Result;

  if (Terminal != NULL)
  {
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
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TScpExplorerForm::UnixPathComboBoxAcceptText(
  TObject * /*Sender*/, UnicodeString & NewText, bool & /*Accept*/)
{
  if (RemoteDirView->Path != NewText)
  {
    if (!TryOpenDirectory(osRemote, NewText))
    {
      FFailedAddress = NewText;
      Abort();
    }
    else
    {
      NewText = RemotePathComboBoxText();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpExplorerForm::UpdateRemotePathComboBox(bool TextOnly)
{
  TCustomScpExplorerForm::UpdateRemotePathComboBox(TextOnly);

  UnixPathComboBox->Text = RemotePathComboBoxText();
}
//---------------------------------------------------------------------------
void __fastcall TScpExplorerForm::RemoteDirViewPathChange(
  TCustomDirView * /*Sender*/)
{
  UpdateRemotePathComboBox(false);
  ResetIncrementalSearch();
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
  DebugFail();
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
void __fastcall TScpExplorerForm::RemoteStatusBarPanelClick(TTBXCustomStatusBar * /*Sender*/,
  TTBXStatusPanel *Panel)
{
  FileStatusBarPanelClick(Panel, osRemote);
}
//---------------------------------------------------------------------------
void __fastcall TScpExplorerForm::GoToAddress()
{
  AddressToolbar->View->Selected = AddressToolbar->View->Find(UnixPathComboBox);
  AddressToolbar->View->EnterToolbarLoop(TTBEnterToolbarLoopOptions() << tbetExecuteSelected);
}
//---------------------------------------------------------------------------
void __fastcall TScpExplorerForm::UpdateImages()
{
  TCustomScpExplorerForm::UpdateImages();

  TImageList * ImageList = ShellImageListForControl(this, ilsSmall);
  UnixPathComboBox->Images = ImageList;
  UnixPathComboBox->SubMenuImages = ImageList;
}
//---------------------------------------------------------------------------
bool TScpExplorerForm::SupportedSession(TSessionData * SessionData)
{
  return !SessionData->IsLocalBrowser;
}
//---------------------------------------------------------------------------
void __fastcall TScpExplorerForm::RemoteOpenDirButtonPopup(TTBCustomItem *, bool DebugUsedArg(FromLink))
{
  CreateOpenDirMenu(RemoteOpenDirButton, osRemote);
}
//---------------------------------------------------------------------------
void TScpExplorerForm::ResetLayoutColumns(TOperationSide)
{
  RemoteDirView->UnixColProperties->ParamsStr = ScpExplorerDirViewParamsDefault;
}
//---------------------------------------------------------------------------
void __fastcall TScpExplorerForm::UpdateControls()
{
  TCustomScpExplorerForm::UpdateControls();

  if ((ActiveControl == NULL) && RemoteDirView->CanFocus())
  {
    ActiveControl = RemoteDirView;
  }

  ColumndsSubmenuItem->Enabled = (RemoteDirView->DirViewStyle == dvsReport);
}
