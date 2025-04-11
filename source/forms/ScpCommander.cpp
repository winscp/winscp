//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "ScpCommander.h"

#include <Common.h>
#include <CoreMain.h>
#include <Interface.h>
#include <TextsCore.h>
#include <TextsWin.h>
#include <HelpWin.h>
#include <VCLCommon.h>
#include <GUITools.h>
#include <DragDrop.hpp>
#include <StrUtils.hpp>
#include <IOUtils.hpp>
#include <DateUtils.hpp>

#include "Glyphs.h"
#include "NonVisual.h"
#include "Tools.h"
#include "WinConfiguration.h"
#include "TerminalManager.h"
#include "Bookmarks.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "CustomDirView"
#pragma link "CustomScpExplorer"
#pragma link "CustomUnixDirView"
#pragma link "IEListView"
#pragma link "NortonLikeListView"
#pragma link "UnixDirView"
#pragma link "DirView"
#pragma link "PathLabel"
#pragma link "HistoryComboBox"
#pragma link "CustomDriveView"
#pragma link "DriveView"
#pragma link "UnixDriveView"
#pragma link "TB2Dock"
#pragma link "TB2Item"
#pragma link "TB2Toolbar"
#pragma link "TBX"
#pragma link "TB2ExtItems"
#pragma link "TBXExtItems"
#pragma link "TBXLists"
#pragma link "TBXStatusBars"
#pragma link "TBXToolPals"
#pragma link "ThemePageControl"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
class TSynchronizedBrowsingGuard
{
public:
  TSynchronizedBrowsingGuard()
  {
    FWasSynchronisingBrowsing = NonVisualDataModule->SynchronizeBrowsingAction2->Checked;
    NonVisualDataModule->SynchronizeBrowsingAction2->Checked = false;
  }

  ~TSynchronizedBrowsingGuard()
  {
    NonVisualDataModule->SynchronizeBrowsingAction2->Checked = FWasSynchronisingBrowsing;
  }

private:
  bool FWasSynchronisingBrowsing;
};
//---------------------------------------------------------------------------
__fastcall TScpCommanderForm::TScpCommanderForm(TComponent* Owner)
        : TCustomScpExplorerForm(Owner)
{
  FConstructed = true;
  FCurrentSide = osLocal;
  FLastLeftPanelWidth = LeftPanelWidth;
  FNormalPanelsWidth = -1;
  FSynchronisingBrowse = false;
  FFirstTerminal = true;
  FInternalDDDownloadList = new TStringList();
  FLocalPathComboBoxPaths = new TStringList();

  LocalPathComboUpdateDrives();

  LocalBackButton->LinkSubitems = HistoryMenu(osLocal, true)->Items;
  LocalForwardButton->LinkSubitems = HistoryMenu(osLocal, false)->Items;
  RemoteBackButton->LinkSubitems = HistoryMenu(osRemote, true)->Items;
  RemoteForwardButton->LinkSubitems = HistoryMenu(osRemote, false)->Items;

  TopDock->PopupMenu = NonVisualDataModule->CommanderBarPopup;
  CopyPopup(StatusBar, TopDock);
  CopyPopup(QueueDock, TopDock);
  CopyPopup(QueueLabel, TopDock);
  CopyPopup(BottomDock, TopDock);
  CopyPopup(QueueSeparatorPanel, TopDock);
  CopyPopup(QueueFileList, TopDock);
  CopyPopup(QueueFileListSplitter, TopDock);

  LocalTopDock->PopupMenu = NonVisualDataModule->LocalPanelPopup;
  CopyPopup(LocalPathLabel, LocalTopDock);
  CopyPopup(LocalStatusBar, LocalTopDock);
  CopyPopup(LocalDriveView, LocalTopDock);
  CopyPopup(LocalBottomDock, LocalTopDock);

  RemoteTopDock->PopupMenu = NonVisualDataModule->RemotePanelPopup;
  CopyPopup(RemotePathLabel, RemoteTopDock);
  CopyPopup(RemoteStatusBar, RemoteTopDock);
  CopyPopup(RemoteDriveView, RemoteTopDock);
  CopyPopup(OtherLocalDriveView, RemoteTopDock);
  CopyPopup(RemoteBottomDock, RemoteTopDock);

  SetShortcuts();
  Splitter->ShowHint = True;
  LocalPanelSplitter->ShowHint = true;
  reinterpret_cast<TLabel*>(Splitter)->OnDblClick = SplitterDblClick;
  reinterpret_cast<TLabel*>(LocalPanelSplitter)->OnDblClick = PanelSplitterDblClick;
  reinterpret_cast<TLabel*>(RemotePanelSplitter)->OnDblClick = PanelSplitterDblClick;

  CommandLineCombo->Text = L"";
  FCommandLineComboPopulated = false;

  for (int i = 0; i < Toolbar2Toolbar->Items->Count; i++)
  {
    UpdateToolbar2ItemCaption(Toolbar2Toolbar->Items->Items[i]);
  }

  UseDesktopFont(LocalDirView);
  UseDesktopFont(LocalDriveView);
  UseDesktopFont(LocalPathLabel);
  UseDesktopFont(RemotePathLabel);
  UseDesktopFont(LocalStatusBar);
  UseDesktopFont(StatusBar);
  UseDesktopFont(OtherLocalDirView);
  UseDesktopFont(OtherLocalDriveView);

  OtherLocalDirView->Align = RemoteDirView->Align;
  OtherLocalDriveView->Align = RemoteDriveView->Align;

  NonVisualDataModule->QueueSpeedComboBoxItem(QueueSpeedComboBoxItem);
  // particularly to reorder panels on right-to-left bidi mode
  ConfigurationChanged();
}
//---------------------------------------------------------------------------
__fastcall TScpCommanderForm::~TScpCommanderForm()
{
  delete FInternalDDDownloadList;
  SAFE_DESTROY(FLocalPathComboBoxPaths);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::UpdateToolbar2ItemCaption(TTBCustomItem * Item)
{
  Item->Caption =
    ShortCutToText(Item->ShortCut) + L" " +
    StripEllipsis(StripHotkey(Item->Caption));
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::RestoreFormParams()
{
  DebugAssert(WinConfiguration);
  TCustomScpExplorerForm::RestoreFormParams();
  RestoreForm(WinConfiguration->ScpCommander.WindowParams, this, false, ScpCommanderWindowParamsDefault);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::RestorePanelParams(
  TCustomDirView * DirView, TControl * DriveControl, TTBXStatusBar * StatusBar,
  const TScpCommanderPanelConfiguration & PanelConfiguration)
{
  DirView->ColProperties->ParamsStr = PanelConfiguration.DirViewParams;
  DirView->DirViewStyle = (TDirViewStyle)PanelConfiguration.ViewStyle;
  StatusBar->Visible = PanelConfiguration.StatusBar;
  DriveControl->Visible = PanelConfiguration.DriveView;
  if (DriveControl->Align == alTop)
  {
    DriveControl->Height = LoadDimension(PanelConfiguration.DriveViewHeight, PanelConfiguration.DriveViewHeightPixelsPerInch, this);
  }
  else
  {
    DriveControl->Width = LoadDimension(PanelConfiguration.DriveViewWidth, PanelConfiguration.DriveViewWidthPixelsPerInch, this);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::RestoreParams()
{
  DebugAssert(Configuration);

  TCustomScpExplorerForm::RestoreParams();
  LeftPanelWidth = WinConfiguration->ScpCommander.LocalPanelWidth;
  LoadToolbarsLayoutStr(WinConfiguration->ScpCommander.ToolbarsLayout, WinConfiguration->ScpCommander.ToolbarsButtons);
  if (IsUWP())
  {
    UpdatesToolbar->Visible = false;
  }
  SessionsPageControl->Visible = WinConfiguration->ScpCommander.SessionsTabs;
  StatusBar->Visible = WinConfiguration->ScpCommander.StatusBar;

  RestorePanelParams(LocalDirView, LocalDriveView, LocalStatusBar, WinConfiguration->ScpCommander.LocalPanel);
  RestorePanelParams(RemoteDirView, RemoteDrivePanel, RemoteStatusBar, WinConfiguration->ScpCommander.RemotePanel);
  OtherLocalDirView->ColProperties->ParamsStr = WinConfiguration->ScpCommander.OtherLocalPanelDirViewParams;
  OtherLocalDirView->DirViewStyle = (TDirViewStyle)WinConfiguration->ScpCommander.OtherLocalPanelViewStyle;
  FPanelsRestored = true;

  // just to make sure
  LocalDirView->DirColProperties->ExtVisible = false;
  RemoteDirView->UnixColProperties->ExtVisible = false;
  OtherLocalDirView->DirColProperties->ExtVisible = false;
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::StorePanelParams(
  TCustomDirView * DirView, TControl * DriveControl, TTBXStatusBar * StatusBar,
  TScpCommanderPanelConfiguration & PanelConfiguration)
{
  PanelConfiguration.DirViewParams = DirView->ColProperties->ParamsStr;
  PanelConfiguration.ViewStyle = DirView->DirViewStyle;
  PanelConfiguration.StatusBar = StatusBar->Visible;
  PanelConfiguration.DriveView = DriveControl->Visible;
  if (DriveControl->Align == alTop)
  {
    PanelConfiguration.DriveViewHeight = DriveControl->Height;
    PanelConfiguration.DriveViewHeightPixelsPerInch = GetControlPixelsPerInch(this);
  }
  else
  {
    PanelConfiguration.DriveViewWidth = DriveControl->Width;
    PanelConfiguration.DriveViewWidthPixelsPerInch = GetControlPixelsPerInch(this);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::StoreParams()
{
  DebugAssert(WinConfiguration);

  WinConfiguration->BeginUpdate();
  try
  {
    SaveCommandLine();

    TScpCommanderConfiguration CommanderConfiguration = WinConfiguration->ScpCommander;
    CommanderConfiguration.ToolbarsLayout = GetToolbarsLayoutStr();
    CommanderConfiguration.ToolbarsButtons = GetToolbarsButtonsStr();
    CommanderConfiguration.LocalPanelWidth = LeftPanelWidth;
    CommanderConfiguration.SessionsTabs = SessionsPageControl->Visible;
    CommanderConfiguration.StatusBar = StatusBar->Visible;

    CommanderConfiguration.CurrentPanel = FCurrentSide;

    StorePanelParams(LocalDirView, LocalDriveView, LocalStatusBar, CommanderConfiguration.LocalPanel);
    StorePanelParams(RemoteDirView, RemoteDrivePanel, RemoteStatusBar, CommanderConfiguration.RemotePanel);
    CommanderConfiguration.OtherLocalPanelDirViewParams = OtherLocalDirView->ColProperties->ParamsStr;
    CommanderConfiguration.OtherLocalPanelViewStyle = reinterpret_cast<TDirViewStyle>(OtherLocalDirView->DirViewStyle);

    CommanderConfiguration.LocalPanel.LastPath = LocalDirView->Path;
    CommanderConfiguration.OtherLocalPanelLastPath = OtherLocalDirView->Path;

    CommanderConfiguration.WindowParams = StoreForm(this);

    WinConfiguration->ScpCommander = CommanderConfiguration;

    TCustomScpExplorerForm::StoreParams();
  }
  __finally
  {
    WinConfiguration->EndUpdate();
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::UpdateSession(TManagedTerminal * Session)
{
  TCustomScpExplorerForm::UpdateSession(Session);

  DebugAssert(LocalDirView != NULL);

  SAFE_DESTROY(Session->LocalExplorerState);
  SAFE_DESTROY(Session->OtherLocalExplorerState);

  if (WinConfiguration->PreservePanelState)
  {
    Session->LocalExplorerState = LocalDirView->SaveState();

    if (IsLocalBrowserMode())
    {
      Session->OtherLocalExplorerState = OtherLocalDirView->SaveState();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::UpdateSessionData(TSessionData * Data)
{
  // Keep in sync with TSessionData::CopyStateData
  TCustomScpExplorerForm::UpdateSessionData(Data);

  DebugAssert(LocalDirView);
  Data->LocalDirectory = LocalDirView->PathName;
  if (IsLocalBrowserMode())
  {
    Data->OtherLocalDirectory = OtherLocalDirView->PathName;
  }
  // Setting both directories makes it a local browser
  DebugAssert(Data->IsLocalBrowser == IsLocalBrowserMode());
  Data->SynchronizeBrowsing = NonVisualDataModule->SynchronizeBrowsingAction2->Checked;
}
//---------------------------------------------------------------------------
bool __fastcall TScpCommanderForm::InternalDDDownload(UnicodeString & TargetDirectory)
{
  DebugAssert(IsFileControl(FDDTargetControl, osLocal));
  DebugAssert(!IsLocalBrowserMode());

  bool Result = false;
  if (FDDTargetControl == LocalDirView)
  {
    if (LocalDirView->DropTarget)
    {
      // when drop target is not directory, it is probably file type, which have
      // associated drop handler (such as ZIP file in WinXP). in this case we
      // must leave drop handling to destination application.
      // ! this check is duplicated in LocalDirViewDDTargetHasDropHandler()
      // for shellex downloads
      if (LocalDirView->ItemIsDirectory(LocalDirView->DropTarget))
      {
        TargetDirectory = LocalDirView->ItemFullFileName(LocalDirView->DropTarget);
        Result = true;
      }
    }
    else
    {
      TargetDirectory = DefaultDownloadTargetDirectory();
      Result = true;
    }
  }
  else if (FDDTargetControl == LocalDriveView)
  {
    DebugAssert(LocalDriveView->DropTarget != NULL);
    TargetDirectory = LocalDriveView->NodePathName(LocalDriveView->DropTarget);
    Result = true;
  }
  else
  {
    DebugFail();
    Abort();
  }

  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TScpCommanderForm::DefaultDownloadTargetDirectory()
{
  return IncludeTrailingBackslash(LocalDirView->Path);
}
//---------------------------------------------------------------------------
bool __fastcall TScpCommanderForm::CopyParamDialog(TTransferDirection Direction,
  TTransferType Type, bool Temp, TStrings * FileList, UnicodeString & TargetDirectory,
  TGUICopyParamType & CopyParam, bool Confirm, bool DragDrop, int Options)
{
  bool Result = false;
  // Temp means d&d here so far, may change in future!
  if (Temp && (Direction == tdToLocal) &&
      IsFileControl(FDDTargetControl, osLocal))
  {
    Result = InternalDDDownload(TargetDirectory);
    if (Result)
    {
      DebugAssert(FileList->Count > 0);
      FInternalDDDownloadList->Assign(FileList);
    }
  }
  else if (!Temp && TargetDirectory.IsEmpty())
  {
    if (Direction == tdToLocal)
    {
      TargetDirectory = DefaultDownloadTargetDirectory();
    }
    else
    {
      TargetDirectory = UnixIncludeTrailingBackslash(RemoteDirView->Path);
    }
  }

  if (!Result)
  {
    Result = TCustomScpExplorerForm::CopyParamDialog(Direction, Type, Temp,
      FileList, TargetDirectory, CopyParam, Confirm, DragDrop, Options);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::DoShow()
{
  AddStartupSequence(L"W");
  // Make sure the RemoteDirView is disabled (if not connected yet)
  // before the focusing below,
  // otherwise we disable the view while setting it focused
  // (UpdateControls gets called within the SetFocus),
  // leading to VCL focus inconsistency with Windows,
  // and the view [anything actually] not getting focused after the session
  // is finally connected
  UpdateControls();

  DoLocalDefaultDirectory(OtherLocalDirView, WinConfiguration->ScpCommander.OtherLocalPanelLastPath, L"P");

  // If we do not call SetFocus on any control before DoShow,
  // no control will get focused on Login dialog
  // (HACK seems like a bug in VCL)
  if ((WinConfiguration->ScpCommander.CurrentPanel == osLocal) || !DirView(osOther)->Enabled)
  {
    LocalDirView->SetFocus();
  }
  else
  {
    DirView(osOther)->SetFocus();
  }

  AddStartupSequence(L"U");
  TCustomScpExplorerForm::DoShow();

  // Need to do do this before the main window really displays.
  // Also the NoSession test in TCustomScpExplorerForm::CMShowingChanged rely on this happening here.
  if (ManagedSession == NULL)
  {
    TTerminalManager::Instance()->NewLocalSession();
  }
  AddStartupSequence(L"O");
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::NeedSession(bool Startup)
{
  if (ManagedSession == NULL)
  {
    // in the LastTerminalClosed, new session should have already been created by GetReplacementForLastSession
    DebugAssert(Startup);

    TTerminalManager::Instance()->NewLocalSession();
  }

  TCustomScpExplorerForm::NeedSession(Startup);
}
//---------------------------------------------------------------------------
TManagedTerminal * TScpCommanderForm::GetReplacementForLastSession()
{
  return TTerminalManager::Instance()->NewLocalBrowser();
}
//---------------------------------------------------------------------------
void TScpCommanderForm::NewTab(TOperationSide Side, bool AllowReverse)
{
  if (Side == osCurrent)
  {
    bool Remote = WinConfiguration->DefaultToNewRemoteTab;
    if (IsKeyPressed(VK_CONTROL) && AllowReverse)
    {
      Remote = !Remote;
    }
    Side = Remote ? osRemote : osLocal;
  }

  if (Side == osRemote)
  {
    TCustomScpExplorerForm::NewTab();
  }
  else
  {
    TTerminalManager::Instance()->NewLocalSession(LocalDirView->PathName, OtherLocalDirView->PathName);
  }
}
//---------------------------------------------------------------------------
Boolean __fastcall TScpCommanderForm::AllowedAction(TAction * Action, TActionAllowed Allowed)
{
  #define FLAG ((TActionFlag)(Action->Tag))
  return
    TCustomScpExplorerForm::AllowedAction(Action, Allowed) &&
    // always require Commander flag
    (FLAG & afCommander) &&
    // if action is execution or update, we don't require any other flag
    // if we check for shortcut, we require proper dirview to be selected
    ((Allowed != aaShortCut) ||
     ((FLAG & afLocal) && (FCurrentSide == osLocal)) ||
     ((FLAG & afRemote) && (FCurrentSide == osRemote))
    );
  #undef FLAG
}
//---------------------------------------------------------------------------
TCustomDirView * __fastcall TScpCommanderForm::DirView(TOperationSide Side)
{
  Side = GetSide(Side);
  if (Side == osLocal)
  {
    return LocalDirView;
  }
  else if (DebugAlwaysTrue(Side == osRemote) && IsLocalBrowserMode())
  {
    return OtherLocalDirView;
  }
  else
  {
    return TCustomScpExplorerForm::DirView(Side);
  }
}
//---------------------------------------------------------------------------
bool TScpCommanderForm::IsLocalBrowserMode()
{
  return OtherLocalDirView->Visible;
}
//---------------------------------------------------------------------------
TCustomDriveView * __fastcall TScpCommanderForm::DriveView(TOperationSide Side)
{
  Side = GetSide(Side);
  if (Side == osLocal)
  {
    return LocalDriveView;
  }
  else if (DebugAlwaysTrue(Side == osRemote) && IsLocalBrowserMode())
  {
    return OtherLocalDriveView;
  }
  else
  {
    return TCustomScpExplorerForm::DriveView(Side);
  }
}
//---------------------------------------------------------------------------
bool TScpCommanderForm::SupportsLocalBrowser()
{
  return true;
}
//---------------------------------------------------------------------------
bool TScpCommanderForm::IsSideLocalBrowser(TOperationSide Side)
{
  return (GetSide(Side) == osLocal) || IsLocalBrowserMode();
}
//---------------------------------------------------------------------------
TCustomDirView * TScpCommanderForm::GetCurrentLocalBrowser()
{
  TOperationSide Side = GetSide(osCurrent);
  if (Side == osLocal)
  {
    return LocalDirView;
  }
  else if (DebugAlwaysTrue(Side == osRemote) && IsLocalBrowserMode())
  {
    return OtherLocalDirView;
  }
  else
  {
    DebugFail();
    Abort();
    return NULL;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TScpCommanderForm::DirViewEnabled(TOperationSide Side)
{
  if (IsSideLocalBrowser(Side))
  {
    return true;
  }
  else
  {
    return TCustomScpExplorerForm::DirViewEnabled(Side);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TScpCommanderForm::IsFileControl(TObject * Control,
  TOperationSide Side)
{
  return
    ((Side == osLocal) &&
     ((Control == LocalDirView) || (Control == LocalDriveView))) ||
    TCustomScpExplorerForm::IsFileControl(Control, Side);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::ReloadLocalDirectory(const UnicodeString Directory)
{
  if (Directory.IsEmpty() || SamePaths(Directory, LocalDirView->Path))
  {
    LocalDirView->ReloadDirectory();
    LocalDriveView->ValidateDirectory(LocalDriveView->Selected);
  }
  // we may postpone reload, when hidden, as an optimization
  if (Directory.IsEmpty() || SamePaths(Directory, OtherLocalDirView->Path))
  {
    OtherLocalDirView->ReloadDirectory();
    OtherLocalDriveView->ValidateDirectory(OtherLocalDriveView->Selected);
  }
  TCustomScpExplorerForm::ReloadLocalDirectory();
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::BatchStart(void *& Storage)
{
  Storage = new bool;
  *static_cast<bool*>(Storage) = LocalDirView->WatchForChanges;
  LocalDirView->WatchForChanges = false;
  LocalDriveView->WatchDirectory = false;
  OtherLocalDirView->WatchForChanges = false;
  OtherLocalDriveView->WatchDirectory = false;

  TCustomScpExplorerForm::BatchStart(Storage);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::BatchEnd(void * Storage)
{
  TCustomScpExplorerForm::BatchEnd(Storage);

  DebugAssert(Storage != NULL);

  LocalDirView->WatchForChanges = *static_cast<bool*>(Storage);
  LocalDriveView->WatchDirectory = LocalDirView->WatchForChanges;
  OtherLocalDirView->WatchForChanges = LocalDirView->WatchForChanges;
  OtherLocalDriveView->WatchDirectory = LocalDirView->WatchForChanges;

  delete Storage;
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::StartingWithoutSession()
{
  TCustomScpExplorerForm::StartingWithoutSession();

  AddStartupSequence(L"H");
  LocalDefaultDirectory();
  AddStartupSequence(L"K");
}
//---------------------------------------------------------------------------
void TScpCommanderForm::RestoreSessionLocalDirView(TDirView * ALocalDirView, const UnicodeString & LocalDirectory)
{
  // We will load completely different directory, so particularly
  // do not attempt to select previously selected directory.
  // TODO: Does not work, as LastPath is restored in PathChanging.
  ALocalDirView->ContinueSession(false);

  // reset home directory
  ALocalDirView->HomeDirectory = L"";

  if (!LocalDirectory.IsEmpty() &&
      (FFirstTerminal || !WinConfiguration->ScpCommander.PreserveLocalDirectory || ManagedSession->LocalBrowser))
  {
    try
    {
      ALocalDirView->Path = LocalDirectory;
    }
    catch(Exception & E)
    {
      if (!ManagedSession->SessionData->UpdateDirectories)
      {
        ManagedSession->ShowExtendedException(&E);
      }
      else
      {
        ALocalDirView->OpenFallbackPath(LocalDirectory);
      }
    }
  }
}
//---------------------------------------------------------------------------
void TScpCommanderForm::AnnounceLocalStates(
  bool RestoreState, bool LocalBrowser, TObject * State, TObject * OtherState)
{
  if (RestoreState)
  {
    LocalDirView->AnnounceState(State);
    if (LocalBrowser)
    {
      OtherLocalDirView->AnnounceState(OtherState);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::SessionChanged(bool Replaced)
{
  NonVisualDataModule->SynchronizeBrowsingAction2->Checked = false;

  TCustomScpExplorerForm::SessionChanged(Replaced);

  if (ManagedSession != NULL)
  {
    bool RestoreState =
      WinConfiguration->PreservePanelState &&
        (!WinConfiguration->ScpCommander.PreserveLocalDirectory || ManagedSession->LocalBrowser);
    AnnounceLocalStates(
      RestoreState, ManagedSession->LocalBrowser,
      ManagedSession->LocalExplorerState, ManagedSession->OtherLocalExplorerState);

    try
    {
      RestoreSessionLocalDirView(LocalDirView, ManagedSession->StateData->LocalDirectory);

      if (ManagedSession->LocalBrowser)
      {
        RestoreSessionLocalDirView(OtherLocalDirView, ManagedSession->StateData->OtherLocalDirectory);
      }

      FFirstTerminal = false;

      // Happens when opening a connection from a command-line (StartingWithoutSession was not called),
      // which does not have a local directory set yet.
      // Or when starting with vanila local browser.
      if (LocalDirView->Path.IsEmpty())
      {
        LocalDefaultDirectory();
      }

      if (WinConfiguration->DefaultDirIsHome &&
          !ManagedSession->SessionData->UpdateDirectories &&
          DebugAlwaysTrue(!ManagedSession->LocalBrowser))
      {
        LocalDirView->HomeDirectory = ManagedSession->SessionData->LocalDirectoryExpanded;
      }

      if (RestoreState)
      {
        LocalDirView->RestoreState(ManagedSession->LocalExplorerState);

        if (ManagedSession->LocalBrowser)
        {
          OtherLocalDirView->RestoreState(ManagedSession->OtherLocalExplorerState);
        }
        NonVisualDataModule->SynchronizeBrowsingAction2->Checked = ManagedSession->StateData->SynchronizeBrowsing;
      }
    }
    __finally
    {
      AnnounceLocalStates(RestoreState, ManagedSession->LocalBrowser, NULL, NULL);
    }

    if (ManagedSession->LocalBrowser)
    {
      // Particularly, when switching from a remote to a local-local session,
      // the other local dir view path may not change at all,
      // so the path combo box would retain the remote path.
      UpdateRemotePathComboBox(false);
    }
  }
}
//---------------------------------------------------------------------------
void TScpCommanderForm::DoLocalDefaultDirectory(
  TDirView * DirView, const UnicodeString & LastPath, const UnicodeString & StartupSequenceTag)
{
  bool DocumentsDir = true;
  if (!LastPath.IsEmpty())
  {
    try
    {
      DirView->Path = LastPath;
      DocumentsDir = false;
    }
    catch (...)
    {
    }
  }

  if (DocumentsDir)
  {
    AddStartupSequence(StartupSequenceTag);
    try
    {
      DirView->HomeDirectory = L"";
      UnicodeString HomeDrive = DriveInfo->GetDriveKey(DirView->HomeDirectory);
      if (DriveInfo->Get(HomeDrive)->DriveType == DRIVE_REMOTE)
      {
        DirView->Path = DriveInfo->AnyValidPath();
      }
      else
      {
        DirView->ExecuteHomeDirectory();
      }
    }
    catch(Exception & E)
    {
      ShowExtendedException(NULL, &E);
      DirView->Path = ExtractFilePath(Application->ExeName);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::LocalDefaultDirectory()
{
  DoLocalDefaultDirectory(LocalDirView, WinConfiguration->ScpCommander.LocalPanel.LastPath, L"L");
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::ConfigurationChanged()
{
  TCustomScpExplorerForm::ConfigurationChanged();
  if (WinConfiguration->DefaultDirIsHome && (ManagedSession != NULL) &&
      !ManagedSession->SessionData->UpdateDirectories)
  {
    LocalDirView->HomeDirectory = ManagedSession->SessionData->LocalDirectoryExpanded;
  }
  else
  {
    LocalDirView->HomeDirectory = L"";
  }

  LocalDirView->DimmHiddenFiles = WinConfiguration->DimmHiddenFiles;
  LocalDriveView->DimmHiddenDirs = WinConfiguration->DimmHiddenFiles;
  OtherLocalDirView->DimmHiddenFiles = WinConfiguration->DimmHiddenFiles;
  OtherLocalDriveView->DimmHiddenDirs = WinConfiguration->DimmHiddenFiles;

  LocalDirView->ShowHiddenFiles = WinConfiguration->ShowHiddenFiles;
  LocalDriveView->ShowHiddenDirs = WinConfiguration->ShowHiddenFiles;
  OtherLocalDirView->ShowHiddenFiles = WinConfiguration->ShowHiddenFiles;
  OtherLocalDriveView->ShowHiddenDirs = WinConfiguration->ShowHiddenFiles;

  LocalDirView->FormatSizeBytes = WinConfiguration->FormatSizeBytes;
  OtherLocalDirView->FormatSizeBytes = WinConfiguration->FormatSizeBytes;

  LocalDirView->ConfirmOverwrite = WinConfiguration->ConfirmOverwriting;
  LocalDriveView->ConfirmOverwrite = WinConfiguration->ConfirmOverwriting;
  OtherLocalDirView->ConfirmOverwrite = WinConfiguration->ConfirmOverwriting;
  OtherLocalDriveView->ConfirmOverwrite = WinConfiguration->ConfirmOverwriting;

  LocalDirView->NortonLike = WinConfiguration->ScpCommander.NortonLikeMode;
  OtherLocalDirView->NortonLike = WinConfiguration->ScpCommander.NortonLikeMode;
  RemoteDirView->NortonLike = WinConfiguration->ScpCommander.NortonLikeMode;

  LocalDirView->NaturalOrderNumericalSorting = WinConfiguration->NaturalOrderNumericalSorting;
  LocalDriveView->NaturalOrderNumericalSorting = WinConfiguration->NaturalOrderNumericalSorting;
  OtherLocalDirView->NaturalOrderNumericalSorting = WinConfiguration->NaturalOrderNumericalSorting;
  OtherLocalDriveView->NaturalOrderNumericalSorting = WinConfiguration->NaturalOrderNumericalSorting;

  LocalDirView->AlwaysSortDirectoriesByName = WinConfiguration->AlwaysSortDirectoriesByName;
  OtherLocalDirView->AlwaysSortDirectoriesByName = WinConfiguration->AlwaysSortDirectoriesByName;

  LocalDirView->TimeoutShellIconRetrieval = WinConfiguration->TimeoutShellIconRetrieval;
  LocalDirView->UseIconUpdateThread = WinConfiguration->UseIconUpdateThread;
  OtherLocalDirView->TimeoutShellIconRetrieval = WinConfiguration->TimeoutShellIconRetrieval;
  OtherLocalDirView->UseIconUpdateThread = WinConfiguration->UseIconUpdateThread;

  UpdateRowSelect(LocalDirView);
  UpdateRowSelect(OtherLocalDirView);

  // See also LocalDirViewDDTargetHasDropHandler
  LocalDirView->DragDropFilesEx->ShellExtensions->DropHandler = !WinConfiguration->DDFakeFile;
  LocalDriveView->DragDropFilesEx->ShellExtensions->DropHandler = !WinConfiguration->DDFakeFile;
  OtherLocalDirView->DragDropFilesEx->ShellExtensions->DropHandler = !WinConfiguration->DDFakeFile;
  OtherLocalDriveView->DragDropFilesEx->ShellExtensions->DropHandler = !WinConfiguration->DDFakeFile;

  if (Panel(true)->Left > Panel(false)->Left)
  {
    DisableAlign();
    try
    {
      int AWidth = ClientWidth;
      Panel(false)->Align = alClient;
      // In bidi mode it gets swapped to alRight, what does not work with the rest of this logic
      Splitter->Align = alLeft;
      Panel(true)->Align = alLeft;
      TControl * ControlsOrder[] =
        { Panel(true), Splitter, Panel(false) };
      SetHorizontalControlsOrder(ControlsOrder, LENOF(ControlsOrder));
      Panel(true)->TabOrder = 0;
      Panel(false)->TabOrder = 1;
      ClientWidth = AWidth;
      LeftPanelWidth = FLastLeftPanelWidth;
    }
    __finally
    {
      EnableAlign();
    }

    int LocalIndex = MenuToolbar->Items->IndexOf(LocalMenuButton);
    int RemoteIndex = MenuToolbar->Items->IndexOf(RemoteMenuButton);
    MenuToolbar->Items->Move(LocalIndex, RemoteIndex);
    RemoteIndex = MenuToolbar->Items->IndexOf(RemoteMenuButton);
    MenuToolbar->Items->Move(RemoteIndex, LocalIndex);

    SWAP(TShortCut, NonVisualDataModule->LocalChangePathAction2->ShortCut, NonVisualDataModule->RemoteChangePathAction2->ShortCut);

    SWAP(UnicodeString, NonVisualDataModule->CommanderLocalPanelAction->Caption, NonVisualDataModule->CommanderRemotePanelAction->Caption);
    SWAP(UnicodeString, NonVisualDataModule->CommanderLocalPanelAction->Hint, NonVisualDataModule->CommanderRemotePanelAction->Hint);
    SWAP(int, NonVisualDataModule->LocalLocalCopyAction->ImageIndex, NonVisualDataModule->LocalOtherCopyAction->ImageIndex);
    SWAP(int, NonVisualDataModule->LocalLocalMoveAction->ImageIndex, NonVisualDataModule->LocalOtherMoveAction->ImageIndex);
  }

  if ((RemoteDrivePanel->Align == alLeft) != WinConfiguration->ScpCommander.TreeOnLeft)
  {
    TScpCommanderPanelConfiguration LocalPanel = WinConfiguration->ScpCommander.LocalPanel;
    TScpCommanderPanelConfiguration RemotePanel = WinConfiguration->ScpCommander.RemotePanel;

    bool TreeOnLeft = WinConfiguration->ScpCommander.TreeOnLeft;

    // save value only if it was set yet
    if (FPanelsRestored)
    {
      if (TreeOnLeft)
      {
        // want to be on left, so it is on top, saving height
        LocalPanel.DriveViewHeight = LocalDriveView->Height;
        RemotePanel.DriveViewHeight = RemoteDrivePanel->Height;
      }
      else
      {
        LocalPanel.DriveViewWidth = LocalDriveView->Width;
        RemotePanel.DriveViewWidth = RemoteDrivePanel->Width;
      }
    }

    TAlign NonClientAlign = (TreeOnLeft ? alLeft : alTop);
    // VCL adjusts cursors only between crVSplit and crHSplit,
    // See TSplitter.RequestAlign
    TCursor SplitterCursor = (TreeOnLeft ? crSizeWE : crSizeNS);
    LocalDriveView->Align = NonClientAlign;
    OtherLocalDriveView->Align = NonClientAlign;
    LocalPanelSplitter->Align = NonClientAlign;
    LocalPanelSplitter->Cursor = SplitterCursor;
    RemoteDrivePanel->Align = NonClientAlign;
    RemotePanelSplitter->Align = NonClientAlign;
    RemotePanelSplitter->Cursor = SplitterCursor;
    FixControlsPlacement();

    if (TreeOnLeft)
    {
      LocalDriveView->Width = LocalPanel.DriveViewWidth;
      RemoteDrivePanel->Width = RemotePanel.DriveViewWidth;
    }
    else
    {
      LocalDriveView->Height = LocalPanel.DriveViewHeight;
      RemoteDrivePanel->Height = RemotePanel.DriveViewHeight;
    }

    // in case it triggers config-changed event (does not),
    // make sure it does only after we apply the TreeOnLeft change to avoid endless recursion
    WinConfiguration->ScpCommander.LocalPanel = LocalPanel;
    WinConfiguration->ScpCommander.RemotePanel = RemotePanel;
  }

  if (FExplorerKeyboardShortcuts != WinConfiguration->ScpCommander.ExplorerKeyboardShortcuts)
  {
    SetShortcuts();
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::SetShortcuts()
{
  // set common norton shorcuts to our actions
  NonVisualDataModule->CommanderShortcuts();
  FExplorerKeyboardShortcuts = WinConfiguration->ScpCommander.ExplorerKeyboardShortcuts;
}
//---------------------------------------------------------------------------
TPanel * __fastcall TScpCommanderForm::Panel(bool Left)
{
  bool SwappedPanels = WinConfiguration->ScpCommander.SwappedPanels;
  if (IsRightToLeft())
  {
    SwappedPanels = !SwappedPanels;
  }
  return (SwappedPanels == Left ? RemotePanel : LocalPanel);
}
//---------------------------------------------------------------------------
TPanel * __fastcall TScpCommanderForm::CurrentPanel()
{
  return (FCurrentSide == osLocal ? LocalPanel : RemotePanel);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::SetLeftPanelWidth(double value)
{
  double Total = LocalPanel->Width + RemotePanel->Width;
  FLeftPanelWidth = value;
  if (value * Total != Panel(true)->Width)
  {
    Panel(true)->Width = static_cast<int>(value * Total);
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
double __fastcall TScpCommanderForm::GetLeftPanelWidth()
{
  return FLeftPanelWidth;
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::SplitterMoved(TObject * /*Sender*/)
{
  double Left = Panel(true)->Width;
  double Total = LocalPanel->Width + RemotePanel->Width;
  FLeftPanelWidth = Left / Total;
  FLastLeftPanelWidth = LeftPanelWidth;
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::SplitterCanResize(TObject * /*Sender*/,
      int &NewSize, bool & /*Accept*/)
{
  // When splitter is drag so far to right, that width contraint of remote panel would
  // be violated, it doesn't stop, but extend form width.
  // Following prevents this behavior.
  if (ClientWidth - NewSize - Splitter->Width < Panel(false)->Constraints->MinWidth)
    NewSize = (ClientWidth - Panel(false)->Constraints->MinWidth - Splitter->Width);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::SplitterDblClick(TObject * /*Sender*/)
{
  LeftPanelWidth = 0.5;
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::PanelSplitterDblClick(TObject * Sender)
{
  TSplitter * Splitter = dynamic_cast<TSplitter *>(Sender);
  DebugAssert(Splitter != NULL);
  TControl * DriveView;
  TControl * OtherDriveView;
  if (Splitter == LocalPanelSplitter)
  {
    DriveView = LocalDriveView;
    OtherDriveView = RemoteDrivePanel;
  }
  else
  {
    DriveView = RemoteDrivePanel;
    OtherDriveView = LocalDriveView;
  }

  bool TreeOnLeft = WinConfiguration->ScpCommander.TreeOnLeft;
  DebugAssert(DriveView->Visible);
  if (OtherDriveView->Visible)
  {
    if (TreeOnLeft)
    {
      DriveView->Width = OtherDriveView->Width;
    }
    else
    {
      DriveView->Height = OtherDriveView->Height;
    }
  }
  else
  {
    if (TreeOnLeft)
    {
      OtherDriveView->Width = DriveView->Width;
    }
    else
    {
      OtherDriveView->Height = DriveView->Height;
    }
    OtherDriveView->Visible = true;
  }

  FixControlsPlacement();
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::SetToolbar2ItemAction(TTBXItem * Item, TBasicAction * Action)
{
  if (Item->Action != Action)
  {
    Item->Action = Action;
    UpdateToolbar2ItemCaption(Item);
  }
}
//---------------------------------------------------------------------------
void TScpCommanderForm::UpdatePanelControls(TCustomDirView * ADirView, TCustomDriveView * ADriveView)
{
  TCustomScpExplorerForm::UpdatePanelControls(ADirView, ADriveView);
  ADirView->AddParentDir = (ADirView->DirViewStyle == dvsReport);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::UpdateControls()
{
  // Before TCustomScpExplorerForm disables them (when disconnecting)
  void * Focus = SaveFocus();

  // When implicitly connecting a not-yet-connected session by switching to its tab opened with workspace,
  // Terminal property is still NULL, but ActiveTerminal is already set.
  // Had we used Terminal for the test, the window would switch to local-local mode temporarily while session is opening.
  bool HasTerminal = (TTerminalManager::Instance()->ActiveTerminal != NULL);
  // Set asap, so that TCustomScpExplorerForm already knows if we are in local-local mode.
  RemoteDirView->Visible = HasTerminal;
  RemoteDriveView->Visible = HasTerminal;
  OtherLocalDirView->Visible = !HasTerminal;
  OtherLocalDriveView->Visible = !HasTerminal;

  TCustomScpExplorerForm::UpdateControls();

  UnicodeString SplitterLongHint = Splitter->Hint;
  int P = SplitterLongHint.Pos(L"|");
  if (P == 0)
  {
    P = SplitterLongHint.Pos(L"\n");
  }
  SplitterLongHint.Delete(1, P);
  Splitter->Hint = FORMAT(L"%0.0f%%\n%s", (LeftPanelWidth*100, SplitterLongHint));
  UnicodeString ACommandLinePromptLabel = LoadStr(COMMAND_LINE_LABEL) + " " +
    (!IsSideLocalBrowser(FCurrentSide) ? L"$" : L">");
  if (CommandLinePromptLabel->Caption != ACommandLinePromptLabel)
  {
    CommandLinePromptLabel->Caption = ACommandLinePromptLabel;
    // command line combo width needs to be updated as caption width has probably changed
    ToolBarResize(CommandLineToolbar);
  }
  UpdatePanelControls(LocalDirView, LocalDriveView);
  UpdatePanelControls(OtherLocalDirView, OtherLocalDriveView);
  LocalDirView->Color = PanelColor();
  LocalDriveView->Color = LocalDirView->Color;
  OtherLocalDirView->Color = LocalDirView->Color;
  OtherLocalDriveView->Color = LocalDirView->Color;
  LocalDirView->Font->Color = GetWindowTextColor(LocalDirView->Color);
  LocalDriveView->Font->Color = LocalDirView->Font->Color;
  OtherLocalDirView->Font->Color = LocalDirView->Font->Color;
  OtherLocalDriveView->Font->Color = LocalDirView->Font->Color;

  LocalColumnsSubmenuItem->Enabled = (DirView(osLocal)->DirViewStyle == dvsReport);
  RemoteColumnsSubmenuItem->Enabled = (DirView(osRemote)->DirViewStyle == dvsReport);

  if (IsLocalBrowserMode())
  {
    CurrentCopyItem->Visible = false;
    if (FCurrentSide == osLocal)
    {
      CurrentCopyToItem->Action = NonVisualDataModule->LocalLocalCopyAction;
      CurrentMoveToItem->Action = NonVisualDataModule->LocalLocalMoveAction;
    }
    else
    {
      CurrentCopyToItem->Action = NonVisualDataModule->LocalOtherCopyAction;
      CurrentMoveToItem->Action = NonVisualDataModule->LocalOtherMoveAction;
    }
    LocalCopyItem->Action = NonVisualDataModule->LocalLocalCopyAction;
    LocalMoveItem->Action = NonVisualDataModule->LocalLocalMoveAction;
    RemoteCopyItem->Action = NonVisualDataModule->LocalOtherCopyAction;
    RemoteMoveItem->Action = NonVisualDataModule->LocalOtherMoveAction;
    SetToolbar2ItemAction(CurrentCopyToolbar2Item, CurrentCopyToItem->Action);
    SetToolbar2ItemAction(CurrentMoveToolbar2Item, CurrentMoveToItem->Action);
  }
  else
  {
    CurrentCopyItem->Visible = true;
    CurrentCopyToItem->Action = NonVisualDataModule->RemoteCopyToAction;
    CurrentMoveToItem->Action = NonVisualDataModule->RemoteMoveToAction;
    if (IsSideLocalBrowser(FCurrentSide))
    {
      CurrentCopyItem->Action = NonVisualDataModule->LocalCopyAction;
      CurrentMoveItem->Action = NonVisualDataModule->LocalMoveAction;
      CurrentCopyNonQueueItem->Action = NonVisualDataModule->LocalCopyNonQueueAction;
      CurrentCopyQueueItem->Action = NonVisualDataModule->LocalCopyQueueAction;
    }
    else
    {
      CurrentCopyItem->Action = NonVisualDataModule->RemoteCopyAction;
      CurrentMoveItem->Action = NonVisualDataModule->RemoteMoveAction;
      CurrentCopyNonQueueItem->Action = NonVisualDataModule->RemoteCopyNonQueueAction;
      CurrentCopyQueueItem->Action = NonVisualDataModule->RemoteCopyQueueAction;
    }
    LocalCopyItem->Action = NonVisualDataModule->LocalCopyAction;
    LocalMoveItem->Action = NonVisualDataModule->LocalMoveAction;
    RemoteCopyItem->Action = NonVisualDataModule->RemoteCopyAction;
    RemoteMoveItem->Action = NonVisualDataModule->RemoteMoveAction;
    SetToolbar2ItemAction(CurrentCopyToolbar2Item, CurrentCopyItem->Action);
    SetToolbar2ItemAction(CurrentMoveToolbar2Item, CurrentMoveItem->Action);
  }

  CommandLineCombo->Enabled = IsSideLocalBrowser(FCurrentSide) || CanConsole();
  CommandLinePromptLabel->Enabled = CommandLineCombo->Enabled;

  // Ad hoc hack to disable the drop down menu, when all its items (and all other buttons on the toolbar) are disabled,
  // otherwise it shines too much on the toolbar.
  RemoteNewSubmenuItem->Enabled = DirViewEnabled(osRemote);

  bool LocalOnLeft = (Panel(true) == LocalPanel);
  LocalMenuButton->Caption = LoadStr(HasTerminal ? LOCAL_MENU_CAPTION : (LocalOnLeft ? LEFT_MENU_CAPTION : RIGHT_MENU_CAPTION));
  RemoteMenuButton->Caption = LoadStr(HasTerminal ? REMOTE_MENU_CAPTION : (LocalOnLeft ? RIGHT_MENU_CAPTION : LEFT_MENU_CAPTION));
  RestoreFocus(Focus);

  if (RemotePathLabel->FocusControl != DirView(osOther))
  {
    if (DebugAlwaysTrue(RemotePathLabel->FocusControl != NULL))
    {
      dynamic_cast<TCustomDirView *>(RemotePathLabel->FocusControl)->PathLabel = NULL;
    }
    DirView(osOther)->PathLabel = RemotePathLabel;
    RemotePathLabel->UnixPath = !IsSideLocalBrowser(osOther);
    DirView(osOther)->UpdateStatusBar();
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::ChangePath(TOperationSide Side)
{
  DebugAssert((Side == osLocal) || (Side == osRemote));
  TTBXComboBoxItem * PathComboBox;
  if (Side == osLocal)
  {
    PathComboBox = LocalPathComboBox;
  }
  else
  {
    PathComboBox = RemotePathComboBox;
  }
  ClickToolbarItem(PathComboBox, false);
}
//---------------------------------------------------------------------------
TControl * __fastcall TScpCommanderForm::GetComponent(Byte Component)
{
  switch (Component) {
    case fcToolBar2: return Toolbar2Toolbar; // name changed to enforce change of default visibility
    case fcStatusBar: return StatusBar;
    case fcLocalStatusBar: return LocalStatusBar;
    case fcRemoteStatusBar: return RemoteStatusBar;
    case fcCommandLinePanel: return CommandLineToolbar;
    case fcLocalTree: return LocalDriveView;
    case fcSessionToolbar: return SessionToolbar2;
    case fcCustomCommandsBand: return CustomCommandsToolbar;
    case fcColorMenu: return reinterpret_cast<TControl*>(ColorMenuItem);
    case fcTransferDropDown: return reinterpret_cast<TControl*>(TransferDropDown);
    case fcTransferList: return reinterpret_cast<TControl*>(TransferList);
    case fcTransferLabel: return reinterpret_cast<TControl*>(TransferLabel);
    case fcLocalPopup: return reinterpret_cast<TControl *>(NonVisualDataModule->LocalFilePopup);
    case fcRemotePathComboBox: return reinterpret_cast<TControl*>(RemotePathComboBox);
    case fcMenu: return MenuToolbar;

    case fcCommanderMenuBand: return MenuToolbar;
    case fcCommanderSessionBand: return SessionToolbar2;
    case fcCommanderPreferencesBand: return PreferencesToolbar;
    case fcCommanderSortBand: return SortToolbar;
    case fcCommanderCommandsBand: return CommandsToolbar;
    case fcCommanderUpdatesBand: return UpdatesToolbar;
    case fcCommanderTransferBand: return TransferToolbar;
    case fcCommanderCustomCommandsBand: return CustomCommandsToolbar;
    case fcCommanderLocalHistoryBand: return LocalHistoryToolbar;
    case fcCommanderLocalNavigationBand: return LocalNavigationToolbar;
    case fcCommanderLocalFileBand: return LocalFileToolbar;
    case fcCommanderLocalSelectionBand: return LocalSelectionToolbar;
    case fcCommanderRemoteHistoryBand: return RemoteHistoryToolbar;
    case fcCommanderRemoteNavigationBand: return RemoteNavigationToolbar;
    case fcCommanderRemoteFileBand: return RemoteFileToolbar;
    case fcCommanderRemoteSelectionBand: return RemoteSelectionToolbar;
    default: return TCustomScpExplorerForm::GetComponent(Component);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::FixControlsPlacement()
{
  TCustomScpExplorerForm::FixControlsPlacement();

  QueueSeparatorPanel->Visible = QueuePanel->Visible;

  LocalPanelSplitter->Visible = LocalDriveView->Visible;

  TControl * ControlsOrder[] =
    { BottomDock, QueueSeparatorPanel, QueueSplitter, QueuePanel, StatusBar };
  SetVerticalControlsOrder(ControlsOrder, LENOF(ControlsOrder));

  TControl * LocalControlsOrder[] =
    { LocalTopDock, LocalPathLabel, LocalDriveView, LocalPanelSplitter,
      LocalDirView, LocalBottomDock, LocalStatusBar };
  SetVerticalControlsOrder(LocalControlsOrder, LENOF(LocalControlsOrder));
  SetHorizontalControlsOrder(LocalControlsOrder, LENOF(LocalControlsOrder));

  TControl * RemoteControlsOrder[] =
    { RemoteTopDock, RemotePathLabel, RemoteDrivePanel, RemotePanelSplitter,
      RemoteDirPanel, RemoteBottomDock, RemoteStatusBar };
  SetVerticalControlsOrder(RemoteControlsOrder, LENOF(RemoteControlsOrder));
  SetHorizontalControlsOrder(RemoteControlsOrder, LENOF(RemoteControlsOrder));

  MakeFocusedItemVisible(LocalDirView);
  MakeFocusedItemVisible(OtherLocalDirView);
}
//---------------------------------------------------------------------------
bool __fastcall TScpCommanderForm::GetHasDirView(TOperationSide Side)
{
  return TCustomScpExplorerForm::GetHasDirView(Side) || (Side == osLocal);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::CompareDirectories()
{
  LocalDirView->CompareFiles(DirView(osOther), false,
    WinConfiguration->ScpCommander.CompareCriterias());
  DirView(osOther)->CompareFiles(LocalDirView, false,
    WinConfiguration->ScpCommander.CompareCriterias());
  if (LocalDirView->SelCount + DirView(osOther)->SelCount == 0)
  {
    UnicodeString Message = MainInstructions(LoadStr(COMPARE_NO_DIFFERENCES));
    MessageDialog(Message, qtInformation, qaOK, HELP_COMPARE_NO_DIFFERENCES);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::SynchronizeDirectories()
{
  DebugAssert(!IsLocalBrowserMode());
  UnicodeString LocalDirectory = LocalDirView->PathName;
  UnicodeString RemoteDirectory = RemoteDirView->PathName;
  DoSynchronizeDirectories(LocalDirectory, RemoteDirectory, -1);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::FullSynchronizeDirectories()
{
  DebugAssert(!IsLocalBrowserMode());
  UnicodeString LocalDirectory = LocalDirView->PathName;
  UnicodeString RemoteDirectory = RemoteDirView->PathName;
  bool SaveMode = !(GUIConfiguration->SynchronizeModeAuto < 0);
  TSynchronizeMode Mode =
    (SaveMode ? (TSynchronizeMode)GUIConfiguration->SynchronizeModeAuto :
      ((FCurrentSide == osLocal) ? smRemote : smLocal));
  int Params = GUIConfiguration->SynchronizeParams;
  if (DoFullSynchronizeDirectories(LocalDirectory, RemoteDirectory, Mode, Params, SaveMode, -1) >= 0)
  {
    if (SaveMode)
    {
      GUIConfiguration->SynchronizeModeAuto = Mode;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::ExploreLocalDirectory(TOperationSide Side)
{
  DebugAssert(IsSideLocalBrowser(Side));
  OpenFolderInExplorer(DirView(Side)->Path);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::LocalDirViewExecFile(TObject *Sender,
      TListItem *Item, bool &AllowExec)
{
  DebugAssert(Item);
  TDirView * ADirView = DebugNotNull(dynamic_cast<TDirView *>(Sender));
  if ((UpperCase(PFileRec(Item->Data)->FileExt) == L"LNK") &&
      DirectoryExists(ApiPath(ResolveFileShortCut(ADirView->ItemFullFileName(Item), true))))
  {
    AllowExec = true;
  }
  else
  {
    DoDirViewExecFile(Sender, Item, AllowExec);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::LocalFileControlDDDragEnter(TObject *Sender,
      IDataObject *DataObj, int grfKeyState, TPoint &Point, int &dwEffect,
      bool &Accept)
{
  // LocalDirViewDDDragEnter is duplication of
  // TCustomScpExplorerForm::DirViewDDDragEnter, but it differs in
  // literal type of 'DataObj' parameter. The actual type is however the same.
  FileControlDDDragEnter(Sender, DataObj, grfKeyState, Point, dwEffect, Accept);
}
//---------------------------------------------------------------------------
bool __fastcall TScpCommanderForm::PanelOperation(TOperationSide Side,
  bool DragDrop)
{
  return TCustomScpExplorerForm::PanelOperation(Side, DragDrop) ||
    (DropSourceControl == LocalDirView);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::FileOperationProgress(
  TFileOperationProgressType & ProgressData)
{
  // Heuristic: When operation finishes and DD target is local dir view,
  // we suppose that drag&drop download finished, so local dir view should be
  // reloaded
  if (!ProgressData.InProgress && FProgressForm &&
      IsFileControl(FDDTargetControl, osLocal) &&
      ProgressData.IsTransfer())
  {
    ReloadLocalDirectory();
  }
  TCustomScpExplorerForm::FileOperationProgress(ProgressData);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TScpCommanderForm::ChangeFilePath(UnicodeString Path, TOperationSide Side)
{
  TGUICopyParamType CopyParams = GUIConfiguration->CurrentCopyParam;
  UnicodeString Result;
  while (!Path.IsEmpty())
  {
    int P = Path.Pos(Side == osLocal ? L'\\' : L'/');
    if (P > 0)
    {
      Result += Terminal->ChangeFileName(&CopyParams, Path.SubString(1, P - 1), Side, false) +
        (Side == osLocal ? L'/' : L'\\');
      Path.Delete(1, P);
    }
    else
    {
      Result += Terminal->ChangeFileName(&CopyParams, Path, osLocal, false);
      Path = L"";
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::CreateRemoteDirectory(const UnicodeString & Path)
{
  UnicodeString Dir = UnixExtractFileDir(Path);
  if (!IsUnixRootPath(Dir) && !Terminal->FileExists(Dir))
  {
    CreateRemoteDirectory(Dir);
  }
  TRemoteProperties Properties = GUIConfiguration->NewDirectoryProperties;
  TCustomScpExplorerForm::CreateRemoteDirectory(Path, Properties);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::SynchronizeBrowsingLocal(
  UnicodeString PrevPath, UnicodeString & NewPath, bool Create)
{
  DebugAssert(!IsLocalBrowserMode());
  Terminal->ExceptionOnFail = true;
  TStrings * Paths = new TStringList();
  try
  {
    Paths->Add(IncludeTrailingBackslash(PrevPath));
    Paths->Add(IncludeTrailingBackslash(LocalDirView->Path));
    UnicodeString CommonPath;
    if (ExtractCommonPath(Paths, CommonPath))
    {
      PrevPath = IncludeTrailingBackslash(PrevPath);
      CommonPath = IncludeTrailingBackslash(CommonPath);
      NewPath = RemoteDirView->Path;
      while (!SamePaths(PrevPath, CommonPath))
      {
        if (NewPath == UnixExcludeTrailingBackslash(NewPath))
        {
          Abort();
        }
        NewPath = UnixExtractFilePath(UnixExcludeTrailingBackslash(NewPath));
        PrevPath = ExtractFilePath(ExcludeTrailingBackslash(PrevPath));
      }

      NewPath = UnixIncludeTrailingBackslash(NewPath) +
        ToUnixPath(LocalDirView->Path.SubString(PrevPath.Length() + 1,
          LocalDirView->Path.Length() - PrevPath.Length()));
    }
    else
    {
      Abort();
    }

    if (Create)
    {
      CreateRemoteDirectory(UnixExcludeTrailingBackslash(NewPath));
    }

    RemoteDirView->Path = NewPath;
  }
  __finally
  {
    Terminal->ExceptionOnFail = false;
    delete Paths;
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::CreateLocalDirectory(const UnicodeString & Path)
{
  DebugAssert(!IsLocalBrowserMode()); // Used for synchronized browsing only
  UnicodeString Dir = ExtractFileDir(Path);
  if (!Dir.IsEmpty() && !DirectoryExists(Dir))
  {
    CreateLocalDirectory(Dir);
  }
  LocalDirView->CreateDirectory(Path);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::SynchronizeBrowsingRemote(
  UnicodeString PrevPath, UnicodeString & NewPath, bool Create)
{
  TStrings * Paths = new TStringList();
  try
  {
    Paths->Add(UnixIncludeTrailingBackslash(PrevPath));
    Paths->Add(UnixIncludeTrailingBackslash(RemoteDirView->Path));
    UnicodeString CommonPath;

    if (UnixExtractCommonPath(Paths, CommonPath))
    {
      PrevPath = UnixIncludeTrailingBackslash(PrevPath);
      CommonPath = UnixIncludeTrailingBackslash(CommonPath);

      UnicodeString NewLocalPath;
      NewPath = ExcludeTrailingBackslash(LocalDirView->Path);
      while (!UnixSamePath(PrevPath, CommonPath))
      {
        NewLocalPath = ExcludeTrailingBackslash(ExtractFileDir(NewPath));
        if (NewLocalPath == NewPath)
        {
          Abort();
        }
        NewPath = NewLocalPath;
        PrevPath = UnixExtractFilePath(UnixExcludeTrailingBackslash(PrevPath));
      }

      NewPath = IncludeTrailingBackslash(NewPath) +
        ChangeFilePath(
          RemoteDirView->Path.SubString(PrevPath.Length() + 1,
            RemoteDirView->Path.Length() - PrevPath.Length()),
          osRemote);
    }
    else
    {
      Abort();
    }
  }
  __finally
  {
    delete Paths;
  }

  if (Create)
  {
    CreateLocalDirectory(ExcludeTrailingBackslash(NewPath));
  }

  LocalDirView->Path = NewPath;
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::SynchronizeBrowsing(TCustomDirView * ADirView,
  UnicodeString PrevPath, UnicodeString & NewPath, bool Create)
{
  if (ADirView == LocalDirView)
  {
    SynchronizeBrowsingLocal(PrevPath, NewPath, Create);
  }
  else
  {
    SynchronizeBrowsingRemote(PrevPath, NewPath, Create);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::SynchronizeBrowsing(TCustomDirView * ADirView)
{
  UnicodeString PrevPath;
  // cannot assign to UnicodeString before the class is constructed,
  // if we do, the value get lost when UnicodeString constructor gets finally called,
  // what results in memory leak in the best case
  if (FConstructed)
  {
    PrevPath = FPrevPath[ADirView == LocalDirView];
    FPrevPath[ADirView == LocalDirView] = ADirView->Path;
  }

  if (!FSynchronisingBrowse && NonVisualDataModule->SynchronizeBrowsingAction2->Checked &&
      !PrevPath.IsEmpty() && PrevPath != ADirView->Path)
  {
    DebugAssert(!IsLocalBrowserMode());
    TValueRestorer<bool> AllowTransferPresetAutoSelectRestorer(FAllowTransferPresetAutoSelect, false);
    TValueRestorer<bool> SynchronisingBrowseRestorer(FSynchronisingBrowse, true);

    try
    {
      UnicodeString NewPath;

      bool Error = false;
      std::unique_ptr<TStrings> ErrorMoreMessages;
      UnicodeString ErrorHelpKeyword;

      try
      {
        SynchronizeBrowsing(ADirView, PrevPath, NewPath, false);
      }
      // EAbort means that we do not know how to synchronize browsing
      // there's no fallback scenario for that
      catch(EAbort &)
      {
        throw;
      }
      catch(Exception & E)
      {
        // what does this say?
        if (Application->Terminated)
        {
          throw;
        }
        else
        {
          Error = true;
          ErrorMoreMessages.reset(ExceptionToMoreMessages(&E));
          ErrorHelpKeyword =
            MergeHelpKeyword(HELP_SYNC_DIR_BROWSE_ERROR, GetExceptionHelpKeyword(&E));
        }
      }

      // this was moved here out of the above catch clause,
      // to avoid deep nesting, what seems to cause some stray access violations
      if (Error)
      {
        if (MoreMessageDialog(FMTLOAD(SYNC_DIR_BROWSE_CREATE2, (NewPath)),
              ErrorMoreMessages.get(), qtConfirmation, qaYes | qaNo,
              ErrorHelpKeyword) == qaYes)
        {
          try
          {
            SynchronizeBrowsing(ADirView, PrevPath, NewPath, true);
          }
          catch(Exception & E)
          {
            if (!Application->Terminated)
            {
              Terminal->ShowExtendedException(&E);
            }
            throw;
          }
        }
        else
        {
          NonVisualDataModule->SynchronizeBrowsingAction2->Checked = false;
        }
      }

    }
    catch(Exception & E)
    {
      NonVisualDataModule->SynchronizeBrowsingAction2->Checked = false;
      // what does this say?
      if (Application->Terminated)
      {
        throw;
      }
      else
      {
        MessageDialog(LoadStr(SYNC_DIR_BROWSE_ERROR), qtInformation, qaOK,
          HELP_SYNC_DIR_BROWSE_ERROR);
      }
    }

    // note the value restorers at the beginning of this block
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::DoDirViewLoaded(TCustomDirView * ADirView)
{
  TCustomScpExplorerForm::DoDirViewLoaded(ADirView);

  UpdateControls();

  // In addition to being an optimization, this prevents synchronized browsing against the other local panel,
  // when the users happens to e.g. download a file to the directory loaded in the panel.
  if (ADirView != OtherLocalDirView)
  {
    SynchronizeBrowsing(ADirView);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::AddEditLink(TOperationSide Side, bool Add)
{
  if (IsSideLocalBrowser(Side))
  {
    bool Edit = false;
    UnicodeString FileName;
    UnicodeString PointTo;
    bool SymbolicLink = true;

    TDirView * ADirView = DebugNotNull(dynamic_cast<TDirView *>(DirView(Side)));
    if (ADirView->ItemFocused != NULL)
    {
      DebugAssert(ADirView->ItemFocused->Data);
      PFileRec FileRec = (PFileRec)ADirView->ItemFocused->Data;

      Edit = !Add && (UpperCase(FileRec->FileExt) == L"LNK");
      if (Edit)
      {
        UnicodeString FullName = ADirView->ItemFullFileName(ADirView->ItemFocused);
        FileName = FullName;
        PointTo = ResolveFileShortCut(FullName, false);
        if (PointTo.IsEmpty())
        {
          throw Exception(FMTLOAD(RESOLVE_SHORTCUT_ERROR, (FullName)));
        }
      }
      else if (!ADirView->ItemIsParentDirectory(ADirView->ItemFocused))
      {
        PointTo = FileRec->FileName;
      }
    }

    if (DoSymlinkDialog(FileName, PointTo, osLocal, SymbolicLink, Edit, false))
    {
      Configuration->Usage->Inc(L"LocalShortcutsCreated");

      DebugAssert(SymbolicLink);
      DebugAssert(!FileName.IsEmpty());
      DebugAssert(!PointTo.IsEmpty());

      if (ExtractFileDrive(FileName) == L"" && FileName[1] != L'\\')
      {
        FileName = IncludeTrailingBackslash(ADirView->PathName) + FileName;
      }
      if (ExtractFileDrive(PointTo) == L"" && PointTo[1] != L'\\')
      {
        PointTo = IncludeTrailingBackslash(ADirView->PathName) + PointTo;
      }
      if (ExtractFileExt(FileName) == L"")
      {
        FileName = FileName + L".lnk";
      }

      if (Edit)
      {
        DeleteFileChecked(FileName);
      }
      if (!CreateFileShortCut(PointTo, FileName, L""))
      {
        throw Exception(CREATE_SHORTCUT_ERROR);
      }
    }
  }
  else
  {
    TCustomScpExplorerForm::AddEditLink(Side, Add);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::DoOpenDirectoryDialog(TOpenDirectoryMode Mode,
  TOperationSide Side)
{
  bool UseLocationProfiles;
  do
  {
    UseLocationProfiles = WinConfiguration->UseLocationProfiles;
    // Location profiles dialog is not ready to be run without terminal
    if (UseLocationProfiles && IsActiveTerminal(Terminal))
    {
      TStrings * LocalDirectories = NULL;
      TStrings * RemoteDirectories = NULL;
      try
      {
        LocalDirectories = CreateVisitedDirectories(osLocal);
        RemoteDirectories = CreateVisitedDirectories(osRemote);

        UnicodeString Local = LocalDirView->PathName;
        UnicodeString Remote = RemoteDirView->PathName;

        if (LocationProfilesDialog(Mode, Side, Local, Remote, LocalDirectories,
              RemoteDirectories, Terminal))
        {
          DoOpenBookmark(Local, Remote);
        }
      }
      __finally
      {
        delete LocalDirectories;
        delete RemoteDirectories;
      }
    }
    else
    {
      TSynchronizedBrowsingGuard SynchronizedBrowsingGuard;
      TCustomScpExplorerForm::DoOpenDirectoryDialog(Mode, Side);
    }

    // for second and further rounds, always do browse only
    Mode = odBrowse;
  }
  while (UseLocationProfiles != WinConfiguration->UseLocationProfiles);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::DoOpenBookmark(UnicodeString Local, UnicodeString Remote)
{
  TSynchronizedBrowsingGuard SynchronizedBrowsingGuard;
  // make sure that whatever path is valid it is opened first and only
  // after that an eventual error is reported
  try
  {
    if (!Local.IsEmpty())
    {
      DebugAssert(!IsLocalBrowserMode());
      LocalDirView->Path = ExpandEnvironmentVariables(Local);
    }
  }
  __finally
  {
    if (!Remote.IsEmpty())
    {
      // While we might get here when the session is closed (from location profiles),
      // it's not a problem as the Path setter is noop then
      RemoteDirView->Path = Remote;
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TScpCommanderForm::OpenBookmark(TOperationSide Side, TBookmark * Bookmark)
{
  bool Result;
  if (WinConfiguration->UseLocationProfiles && !IsLocalBrowserMode())
  {
    DoOpenBookmark(Bookmark->Local, Bookmark->Remote);
    Result = true;
  }
  else
  {
    Result = TCustomScpExplorerForm::OpenBookmark(Side, Bookmark);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::LocalDirViewDDTargetHasDropHandler(
  TObject * Sender, TListItem * Item, int & /*Effect*/, bool & DropHandler)
{
  // When drop target is not directory, it is probably file type, which have
  // associated drop handler (such as EXE file). In this case we
  // cannot allow drop when when using shellex,
  // as drop handlers are disabled, so drop would error
  // (see TShellExtension.DropHandler assignment in ConfigurationChanged),
  if (WinConfiguration->DDFakeFile &&
      !DebugNotNull(dynamic_cast<TDirView *>(Sender))->ItemIsDirectory(Item))
  {
    DropHandler = false;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TScpCommanderForm::DDGetTarget(
  UnicodeString & Directory, bool & ForceQueue, UnicodeString & CounterName)
{
  bool Result;
  if (!FDDFakeFileTarget.IsEmpty())
  {
    Directory = FDDFakeFileTarget;
    FDDFakeFileTarget = L"";
    Result = true;
    CounterName = L"DownloadsDragDropInternal";
    ForceQueue = false;
  }
  else
  {
    Result = TCustomScpExplorerForm::DDGetTarget(Directory, ForceQueue, CounterName);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::DDFakeFileInitDrag(TFileList * FileList,
  bool & Created)
{
  FDDFakeFileTarget = L"";
  TCustomScpExplorerForm::DDFakeFileInitDrag(FileList, Created);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::LocalFileControlDDFileOperation(
  TObject * Sender, int dwEffect, UnicodeString SourcePath,
  UnicodeString TargetPath, bool Paste, bool & DoOperation)
{
  if (IsFileControl(DropSourceControl, osRemote) && !IsLocalBrowserMode())
  {
    UnicodeString TargetDirectory;
    if (InternalDDDownload(TargetDirectory))
    {
      // See TCustomScpExplorerForm::QueueDDProcessDropped
      if (FDDExtMapFile != NULL)
      {
        FDDFakeFileTarget = TargetDirectory;
      }
      else
      {
        DebugAssert(FInternalDDDownloadList->Count > 0);
        DebugAssert(dwEffect == DROPEFFECT_COPY || dwEffect == DROPEFFECT_MOVE);
        TGUICopyParamType CopyParams = GUIConfiguration->CurrentCopyParam;
        TTransferType TransferType = dwEffect == DROPEFFECT_COPY ? ttCopy : ttMove;
        int Options =
          FLAGMASK(DraggingAllFilesFromDirView(osRemote, FInternalDDDownloadList), coAllFiles);
        bool NoConfirmation = Paste ? false : (WinConfiguration->DDTransferConfirmation == asOff);
        if (CopyParamDialog(tdToLocal, TransferType,
              false, FInternalDDDownloadList, TargetDirectory, CopyParams, NoConfirmation, true, Options))
        {
          int Params =
            (TransferType == ttMove ? cpDelete : 0);
          DDDownload(FInternalDDDownloadList, TargetDirectory,
            &CopyParams, Params);
          Configuration->Usage->Inc(L"DownloadsDragDropInternal");
          FInternalDDDownloadList->Clear();
        }
      }
      DoOperation = false;
    }
  }

  if (IsLocalBrowserMode() &&
      ((IsFileControl(Sender, osRemote) && IsFileControl(DropSourceControl, osLocal)) ||
       (IsFileControl(Sender, osLocal) && IsFileControl(DropSourceControl, osRemote))))
  {
    if (dwEffect == DROPEFFECT_MOVE)
    {
      Configuration->Usage->Inc(L"LocalLocalMovesDragDropInterpanel");
    }
    else
    {
      Configuration->Usage->Inc(L"LocalLocalCopiesDragDropInterpanel");
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::RemoteFileControlDDFileOperationExecuted(
  TObject * /*Sender*/, int dwEffect, UnicodeString /*SourcePath*/,
  UnicodeString /*TargetPath*/)
{
  if ((dwEffect == DROPEFFECT_MOVE) &&
      IsFileControl(DropSourceControl, osLocal))
  {
    ReloadLocalDirectory();
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::LocalDirViewEnter(TObject * /*Sender*/)
{
  SideEnter(osLocal);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::LocalDriveViewEnter(TObject * /*Sender*/)
{
  MakeNextInTabOrder(LocalDirView, LocalDriveView);
  SideEnter(osLocal);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::SideEnter(TOperationSide Side)
{
  if (Visible && (IsSideLocalBrowser(FCurrentSide) != IsSideLocalBrowser(Side)))
  {
    // this may get called yet before controls are initialized
    CommandLineCombo->Strings->Clear();
    FCommandLineComboPopulated = false;
  }
  TCustomScpExplorerForm::SideEnter(Side);
  if (Visible)
  {
    UpdateControls();
    UpdatePanelsPathLabelsStatus();
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::UpdatePanelsPathLabelsStatus()
{
  LocalPathLabel->UpdateStatus();
  RemotePathLabel->UpdateStatus();
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::OpenConsole(UnicodeString Command)
{
  SaveCommandLine();
  try
  {
    TCustomScpExplorerForm::OpenConsole(Command);
  }
  __finally
  {
    FCommandLineComboPopulated = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::SaveCommandLine()
{
  if (FCommandLineComboPopulated)
  {
    CustomWinConfiguration->History[
      !IsSideLocalBrowser(FCurrentSide) ? L"Commands" : L"LocalCommands"] =
        CommandLineCombo->Strings;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TScpCommanderForm::ExecuteCommandLine()
{
  UnicodeString Command = CommandLineCombo->Text;
  bool Result =
    !NonVisualDataModule->Busy &&
    !Command.IsEmpty() &&
      (IsSideLocalBrowser(FCurrentSide) ||
       (Terminal->AllowedAnyCommand(Command) &&
        EnsureCommandSessionFallback(fcAnyCommand)));
  if (Result)
  {
    CommandLinePopulate();
    SaveToHistory(CommandLineCombo->Strings, Command);
    CommandLineCombo->Text = L"";
    if (!IsSideLocalBrowser(FCurrentSide))
    {
      OpenConsole(Command);
    }
    else
    {
      ExecuteShellChecked(Command);
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::CommandLinePopulate()
{
  if (!FCommandLineComboPopulated)
  {
    TStrings * CommandsHistory;
    CommandsHistory = CustomWinConfiguration->History[
      !IsSideLocalBrowser(FCurrentSide) ? L"Commands" : L"LocalCommands"];
    if ((CommandsHistory != NULL) && (CommandsHistory->Count > 0))
    {
      CommandLineCombo->Strings = CommandsHistory;
    }
    else
    {
      CommandLineCombo->Strings->Clear();
    }
    FCommandLineComboPopulated = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::GoToCommandLine()
{
  ComponentVisible[fcCommandLinePanel] = true;
  if (CommandLineCombo->Enabled)
  {
    ClickToolbarItem(CommandLineCombo, true);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::GoToTree()
{
  if (FCurrentSide == osLocal)
  {
    ComponentVisible[fcLocalTree] = true;
    LocalDriveView->SetFocus();
  }
  else
  {
    TCustomScpExplorerForm::GoToTree();
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::PanelExportStore(TOperationSide Side,
  TPanelExport Export, TPanelExportDestination Destination,
  TStrings * ExportData)
{
  if (Destination == pedCommandLine)
  {
    ComponentVisible[fcCommandLinePanel] = true;

    UnicodeString Buf;
    for (int Index = 0; Index < ExportData->Count; Index++)
    {
      Buf += ExportData->Strings[Index] + L" ";
    }

    CommandLineCombo->Text = CommandLineCombo->Text + Buf;
  }
  else
  {
    TCustomScpExplorerForm::PanelExportStore(Side, Export, Destination, ExportData);
  }
}
//---------------------------------------------------------------------------
int __fastcall TScpCommanderForm::GetStaticComponentsHeight()
{
  return TCustomScpExplorerForm::GetStaticComponentsHeight() +
    (BottomDock->Visible ? BottomDock->Height : 0) +
    (QueueSeparatorPanel->Visible ? QueueSeparatorPanel->Height : 0) +
    (StatusBar->Visible ? StatusBar->Height : 0);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::SysResizing(unsigned int Cmd)
{
  TCustomScpExplorerForm::SysResizing(Cmd);

  if ((Cmd == SC_MAXIMIZE) ||
      ((Cmd == SC_DEFAULT) && (WindowState != wsMaximized)))
  {
    FNormalPanelsWidth = LocalPanel->Width + RemotePanel->Width;
  }
  else if ((Cmd == SC_RESTORE) ||
    ((Cmd == SC_DEFAULT) && (WindowState == wsMaximized)))
  {
    if (FNormalPanelsWidth >= 0)
    {
      Panel(true)->Width = static_cast<int>(FLeftPanelWidth * FNormalPanelsWidth);
      FNormalPanelsWidth = -1;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::Resize()
{
  TCustomScpExplorerForm::Resize();

  LeftPanelWidth = FLastLeftPanelWidth;
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::PathLabelDblClick(TObject * Sender)
{
  if (!NonVisualDataModule->Busy)
  {
    OpenDirectory(Sender == LocalPathLabel ? osLocal : osRemote);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::LocalPathLabelGetStatus(
  TCustomPathLabel * /*Sender*/, bool & Active)
{
  // WORKAROUND this strange form is here to make borland compiler work :-)
  Active = Active || LocalDriveView->Focused();
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::RemotePathLabelGetStatus(
  TCustomPathLabel * /*Sender*/, bool & Active)
{
  // WORKAROUND this strange form is here to make borland compiler work :-)
  Active = Active || DriveView(osOther)->Focused();
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::DoPathLabelPathClick(TOperationSide Side, const UnicodeString & Path)
{
  if (!NonVisualDataModule->Busy)
  {
    TCustomDirView * ADirView = DirView(Side);

    bool Same;
    if (IsSideLocalBrowser(Side))
    {
      Same = SamePaths(Path, ADirView->Path);
    }
    else
    {
      Same = UnixSamePath(Path, ADirView->Path);
    }

    if (Same)
    {
      OpenDirectory(Side);
    }
    else
    {
      ADirView->Path = Path;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::LocalPathLabelPathClick(
  TCustomPathLabel * /*Sender*/, UnicodeString Path)
{
  DoPathLabelPathClick(osLocal, Path);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::RemotePathLabelPathClick(
  TCustomPathLabel * /*Sender*/, UnicodeString Path)
{
  DoPathLabelPathClick(osOther, Path);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::LocalDirViewFileIconForName(
  TObject * /*Sender*/, UnicodeString & FileName)
{
  int PartialFileExtLen = GetPartialFileExtLen(FileName);
  if (PartialFileExtLen > 0)
  {
    FileName.SetLength(FileName.Length() - PartialFileExtLen);
  }
  if (WinConfiguration->LocalIconsByExt)
  {
    FileName = ExtractFileName(FileName);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::DoUpdateFileStatusBar(
  TObject * Sender, TTBXStatusBar * StatusBar, const TStatusFileInfo & FileInfo, TOperationSide Side)
{
  TCustomDirView * DirView = dynamic_cast<TCustomDirView *>(Sender);
  // Prevent hidden "right" panel from messing with the status bar
  if (DirView->Visible)
  {
    UpdateFileStatusBar(StatusBar, FileInfo, Side);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::LocalDirViewUpdateStatusBar(TObject * Sender, const TStatusFileInfo & FileInfo)
{
  DoUpdateFileStatusBar(Sender, LocalStatusBar, FileInfo, osLocal);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::RemoteDirViewUpdateStatusBar(TObject * Sender, const TStatusFileInfo & FileInfo)
{
  DoUpdateFileStatusBar(Sender, RemoteStatusBar, FileInfo, osRemote);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::OtherLocalDirViewUpdateStatusBar(TObject * Sender, const TStatusFileInfo & FileInfo)
{
  DoUpdateFileStatusBar(Sender, RemoteStatusBar, FileInfo, osOther);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::LocalStatusBarClick(TObject * /*Sender*/)
{
  LocalDirView->SetFocus();
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TScpCommanderForm::PathForCaption()
{
  UnicodeString Result;
  if (IsSideLocalBrowser(FCurrentSide))
  {
    // make sure the path corresponds to the terminal in title
    // (there's a mismatch, while opening a new session)
    if (ManagedSession == TTerminalManager::Instance()->ActiveSession)
    {
      switch (WinConfiguration->PathInCaption)
      {
        case picShort:
          Result = ExtractShortName(GetCurrentLocalBrowser()->PathName, false);
          break;

        case picFull:
          Result = GetCurrentLocalBrowser()->PathName;
          break;
      }
    }
  }
  else
  {
    Result = TCustomScpExplorerForm::PathForCaption();
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::GetTransferPresetAutoSelectData(
  TCopyParamRuleData & Data)
{
  TCustomScpExplorerForm::GetTransferPresetAutoSelectData(Data);
  Data.LocalDirectory = LocalDirView->PathName;
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::RemoteDirViewPathChange(TCustomDirView * /*Sender*/)
{
  UpdateRemotePathComboBox(false);
  ResetIncrementalSearch();
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::UpdateImages()
{
  TCustomScpExplorerForm::UpdateImages();

  TImageList * ImageList = ShellImageListForControl(this, ilsSmall);
  RemotePathComboBox->Images = ImageList;
  RemotePathComboBox->SubMenuImages = ImageList;
  LocalPathComboBox->Images = ImageList;
  LocalPathComboBox->SubMenuImages = ImageList;
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::LocalPathComboUpdateDrives()
{
  FLocalSpecialPaths = 0;
  TStrings* Strings = LocalPathComboBox->Strings;
  Strings->BeginUpdate();
  try
  {
    Strings->Clear();
    FLocalPathComboBoxPaths->Clear();
    Strings->Add(LoadStr(SPECIAL_FOLDER_MY_DOCUMENTS));
    FLocalPathComboBoxPaths->AddObject(GetPersonalFolder(),
      (TObject *)DriveInfo->SpecialFolder[CSIDL_PERSONAL]->ImageIndex);
    FLocalSpecialPaths++;
    Strings->Add(LoadStr(SPECIAL_FOLDER_DESKTOP));
    FLocalPathComboBoxPaths->AddObject(GetDesktopFolder(),
      (TObject *)DriveInfo->SpecialFolder[CSIDL_DESKTOP]->ImageIndex);
    FLocalSpecialPaths++;

    std::unique_ptr<TStrings> Drives(LocalDriveView->GetDrives());
    for (int Index = 0; Index < Drives->Count; Index++)
    {
      UnicodeString Drive = Drives->Strings[Index];
      if (DriveInfo->Get(Drive)->Valid)
      {
        UnicodeString Caption = DriveInfo->GetPrettyName(Drive);
        if (DriveInfo->IsRealDrive(Drive))
        {
          Caption.Insert(L"&", 0);
        }
        Strings->Add(Caption);
        UnicodeString RootPath = DriveInfo->GetDriveRoot(Drive);
        int ImageIndex = DriveInfo->GetImageIndex(Drive);
        FLocalPathComboBoxPaths->AddObject(RootPath, reinterpret_cast<TObject *>(ImageIndex));
      }
    }
  }
  __finally
  {
    Strings->EndUpdate();
  }

  if (IsLocalBrowserMode())
  {
    UpdateRemotePathComboBox(false);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::LocalPathComboUpdate(TCustomDirView * ADirView, TTBXComboBoxItem * PathComboBox)
{
  // this may get called even after destructor finishes
  // (e.g. from SetDockAllowDrag invoked [indirectly] from StoreParams)
  if (FLocalPathComboBoxPaths != NULL)
  {
    DebugAssert(FLocalPathComboBoxPaths->Count == PathComboBox->Strings->Count);

    int Index = 0;
    while ((Index < FLocalPathComboBoxPaths->Count) &&
           !SamePaths(FLocalPathComboBoxPaths->Strings[Index],
             ADirView->Path.SubString(1, FLocalPathComboBoxPaths->Strings[Index].Length())))
    {
      Index++;
    }

    // what to do if not?
    if (Index < FLocalPathComboBoxPaths->Count)
    {
      PathComboBox->ItemIndex = Index;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::DoLocalDirViewPathChange(TCustomDirView * Sender, TTBXComboBoxItem * PathComboBox)
{
  LocalPathComboUpdate(Sender, PathComboBox);
  ResetIncrementalSearch();
  if (IsUncPath(Sender->Path))
  {
    Configuration->Usage->Inc(L"BrowsedUncPath");
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::LocalDirViewPathChange(TCustomDirView * Sender)
{
  DoLocalDirViewPathChange(Sender, LocalPathComboBox);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::LocalPathComboBoxCancel(TObject * /*Sender*/)
{
  LocalPathComboUpdate(LocalDirView, LocalPathComboBox);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::DoLocalPathComboBoxAdjustImageIndex(
  TTBXComboBoxItem * Sender, const UnicodeString AText, int AIndex, int & ImageIndex)
{
  // this may get called even before constructor starts
  // (e.g. from FixControlsPlacement)
  if (FLocalPathComboBoxPaths != NULL)
  {
    TTBXComboBoxItem * PathComboBox = DebugNotNull(dynamic_cast<TTBXComboBoxItem *>(Sender));
    DebugAssert(FSessionChanging || (FLocalPathComboBoxPaths->Count == PathComboBox->Strings->Count));
    DebugAssert(FSessionChanging || (AIndex < FLocalPathComboBoxPaths->Count));

    if (AIndex < 0)
    {
      AIndex = PathComboBox->ItemIndex;
    }

    // We might get called via some Windows messages while switching from remote to local tab (FSessionChanging),
    // before the UpdateRemotePathComboBox gets called. If that happens, ignore the call.
    if ((AIndex >= 0) && (AIndex < FLocalPathComboBoxPaths->Count))
    {
      ImageIndex = int(FLocalPathComboBoxPaths->Objects[AIndex]);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::LocalPathComboBoxAdjustImageIndex(
  TTBXComboBoxItem * Sender, const UnicodeString AText, int AIndex,
  int & ImageIndex)
{
  DoLocalPathComboBoxAdjustImageIndex(Sender, AText, AIndex, ImageIndex);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::DoLocalPathComboBoxItemClick(TDirView * ADirView, TTBXComboBoxItem * PathComboBox)
{
  DebugAssert(FLocalPathComboBoxPaths->Count == PathComboBox->Strings->Count);
  DebugAssert((PathComboBox->ItemIndex >= 0) && (PathComboBox->ItemIndex < FLocalPathComboBoxPaths->Count));

  UnicodeString Path = FLocalPathComboBoxPaths->Strings[PathComboBox->ItemIndex];
  try
  {
    if (PathComboBox->ItemIndex >= FLocalSpecialPaths)
    {
      UnicodeString Drive = DriveInfo->GetDriveKey(Path);
      TDirView * CurrentDirView = dynamic_cast<TDirView *>(DirView(osCurrent));
      if (IsLocalBrowserMode() && (ADirView != CurrentDirView) &&
        SamePaths(DriveInfo->GetDriveKey(CurrentDirView->PathName), Drive))
      {
        ADirView->Path = CurrentDirView->PathName;
      }
      else
      {
        ADirView->ExecuteDrive(Drive);
      }
    }
    else
    {
      ADirView->Path = Path;
    }
  }
  catch (...)
  {
    // Changing the path failed, reset the combo box back.
    // Does not recurse, so infinite recursion should not happen.
    LocalPathComboUpdate(ADirView, PathComboBox);
    throw;
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::LocalPathComboBoxItemClick(TObject *)
{
  DoLocalPathComboBoxItemClick(LocalDirView, LocalPathComboBox);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::ToolbarItemResize(TTBXCustomDropDownItem * Item, int Width)
{
  TCustomScpExplorerForm::ToolbarItemResize(Item, Width);
  if ((Item == LocalPathComboBox) ||
      (Item == RemotePathComboBox) ||
      (Item == CommandLineCombo))
  {
    dynamic_cast<TTBXComboBoxItem *>(Item)->MinListWidth = Width - 4;
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::CommandLineComboPopup(
  TTBCustomItem * /*Sender*/, bool /*FromLink*/)
{
  CommandLinePopulate();
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::CommandLineComboBeginEdit(
  TTBEditItem * /*Sender*/, TTBEditItemViewer * /*Viewer*/, TEdit *EditControl)
{
  InstallPathWordBreakProc(EditControl);
  FCommandLineComboEdit = EditControl;
  FToolbarEditOldWndProc = EditControl->WindowProc;
  EditControl->WindowProc = CommandLineComboEditWndProc;
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::ExitToolbar()
{
  CurrentPanel()->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::CommandLineComboEditWndProc(TMessage & Message)
{
  bool Handled = false;
  if (Message.Msg == WM_CHAR)
  {
    switch (reinterpret_cast<TWMChar &>(Message).CharCode)
    {
      case VK_ESCAPE:
        CommandLineCombo->Text = L"";
        ExitToolbar();
        Handled = true;
        break;

      case VK_TAB:
        CommandLineCombo->Text = FCommandLineComboEdit->Text;
        ExitToolbar();
        Handled = true;
        break;

      case VK_RETURN:
        CommandLineCombo->Text = FCommandLineComboEdit->Text;
        ExitToolbar();
        if (!ExecuteCommandLine())
        {
          // re-enter command line
          // (most probably exited by now as message dialog was shown)
          GoToCommandLine();
        }
        Handled = true;
        break;
    }
  }

  if (!Handled)
  {
    FToolbarEditOldWndProc(Message);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::LocalDriveViewRefreshDrives(TObject * Sender, bool Global)
{
  // Process global drive notifications from only one drive view
  if (!Global || (Sender == LocalDriveView))
  {
    LocalPathComboUpdateDrives();
    LocalPathComboUpdate(LocalDirView, LocalPathComboBox);
    if (IsLocalBrowserMode())
    {
      LocalPathComboUpdate(OtherLocalDirView, RemotePathComboBox);
    }
  }

  if (!Global)
  {
    TDriveView * TheOtherLocalDriveView;
    if (Sender == LocalDriveView)
    {
      TheOtherLocalDriveView = OtherLocalDriveView;
    }
    else
    {
      DebugAssert(Sender == OtherLocalDriveView);
      TheOtherLocalDriveView = LocalDriveView;
    }
    // Or rather an immediate refresh?
    TheOtherLocalDriveView->ScheduleDriveRefresh();
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::HomeDirectory(TOperationSide Side)
{
  bool SynchronizeBrowsing = NonVisualDataModule->SynchronizeBrowsingAction2->Checked;

  TSynchronizedBrowsingGuard SynchronizedBrowsingGuard;

  TCustomScpExplorerForm::HomeDirectory(Side);

  if (SynchronizeBrowsing)
  {
    TCustomScpExplorerForm::HomeDirectory(GetOtherSize(Side));
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::QueueSubmenuItemPopup(
  TTBCustomItem * /*Sender*/, bool /*FromLink*/)
{
  NonVisualDataModule->QueueSpeedComboBoxItemUpdate(QueueSpeedComboBoxItem);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::DoFocusRemotePath(TTerminal * Terminal, const UnicodeString & Path)
{
  TSynchronizedBrowsingGuard SynchronizedBrowsingGuard;
  TCustomScpExplorerForm::DoFocusRemotePath(Terminal, Path);
}
//---------------------------------------------------------------------------
TOperationSide __fastcall TScpCommanderForm::GetOtherSize(TOperationSide Side)
{
  Side = GetSide(Side);
  return ReverseOperationSide(Side);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::HistoryGo(TOperationSide Side, int Index)
{
  TOperationSide OtherSide = GetOtherSize(Side);
  if (NonVisualDataModule->SynchronizeBrowsingAction2->Checked &&
      ((Index < 0) ? (-Index < DirView(OtherSide)->BackCount) : (Index < DirView(OtherSide)->ForwardCount)))
  {
    TSynchronizedBrowsingGuard SynchronizedBrowsingGuard;
    TCustomScpExplorerForm::HistoryGo(Side, Index);
    TCustomScpExplorerForm::HistoryGo(OtherSide, Index);
  }
  else
  {
    TCustomScpExplorerForm::HistoryGo(Side, Index);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::DirViewHistoryGo(
  TCustomDirView * Sender, int Index, bool & Cancel)
{
  TOperationSide Side = (Sender == DirView(osRemote) ? osRemote : osLocal);
  HistoryGo(Side, Index);
  Cancel = true;
}
//---------------------------------------------------------------------------
bool __fastcall TScpCommanderForm::EligibleForImageDisplayMode(TTBCustomItem * Item)
{
  return
    TCustomScpExplorerForm::EligibleForImageDisplayMode(Item) &&
    ((Item->Parent == NULL) || (Item->Parent->ParentComponent != Toolbar2Toolbar));
}
//---------------------------------------------------------------------------
bool __fastcall TScpCommanderForm::UpdateToolbarDisplayMode()
{
  bool Result = TCustomScpExplorerForm::UpdateToolbarDisplayMode();
  if (Result)
  {
    // command line combo width needs to be updated as caption visibility has changed
    ToolBarResize(CommandLineToolbar);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::QueueLabelUpdateStatus()
{
  TCustomScpExplorerForm::QueueLabelUpdateStatus();
  // this is here to deactivate panels path labels when moving focus from
  // directory tree to queue
  UpdatePanelsPathLabelsStatus();
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::DoLocalDirViewContextPopup(TOperationSide Side, TPoint & MousePos, bool & Handled)
{
  if (!WinConfiguration->ScpCommander.SystemContextMenu && !FForceSystemContextMenu)
  {
    DirViewContextPopupDefaultItem(Side, NonVisualDataModule->LocalOpenMenuItem, rdcaOpen, rdcaChangeDir);
    DirViewContextPopupDefaultItem(Side, NonVisualDataModule->LocalEditMenuItem, rdcaEdit, rdcaNone);
    if (IsLocalBrowserMode())
    {
      DirViewContextPopupDefaultItem(Side, NonVisualDataModule->LocalLocalCopyMenuItem, rdcaCopy, rdcaNone);
    }
    else
    {
      DirViewContextPopupDefaultItem(Side, NonVisualDataModule->LocalCopyMenuItem, rdcaCopy, rdcaNone);
    }

    DirViewContextPopup(Side, fcLocalPopup, MousePos);
    Handled = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::LocalDirViewContextPopup(TObject *, TPoint & MousePos, bool & Handled)
{
  DoLocalDirViewContextPopup(osLocal, MousePos, Handled);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::OtherLocalDirViewContextPopup(TObject *, TPoint & MousePos, bool & Handled)
{
  DoLocalDirViewContextPopup(osOther, MousePos, Handled);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::DisplaySystemContextMenu()
{
  TAutoFlag Flag(FForceSystemContextMenu);
  if ((FLastContextPopupScreenPoint.x >= 0) && (FLastContextPopupScreenPoint.Y >= 0))
  {
    DirView(osCurrent)->DisplayContextMenu(FLastContextPopupScreenPoint);
  }
  else
  {
    DirView(osCurrent)->DisplayContextMenuInSitu();
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::LocalStatusBarPanelClick(TTBXCustomStatusBar * /*Sender*/,
  TTBXStatusPanel * Panel)
{
  FileStatusBarPanelClick(Panel, osLocal);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::RemoteStatusBarPanelClick(TTBXCustomStatusBar * /*Sender*/,
  TTBXStatusPanel *Panel)
{
  FileStatusBarPanelClick(Panel, osRemote);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::GoToAddress()
{
  OpenDirectory(GetSide(osCurrent));
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::RemotePathLabelMaskClick(TObject * /*Sender*/)
{
  Filter(osRemote);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::LocalPathLabelMaskClick(TObject * /*Sender*/)
{
  Filter(osLocal);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::LocalOpenDirButtonPopup(TTBCustomItem * /*Sender*/, bool /*FromLink*/)
{
  CreateOpenDirMenu(LocalOpenDirButton, osLocal);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::RemoteOpenDirButtonPopup(TTBCustomItem * /*Sender*/, bool /*FromLink*/)
{
  CreateOpenDirMenu(RemoteOpenDirButton, osRemote);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::CopyFilesToClipboard(TOperationSide Side, bool OnFocused)
{
  if (IsSideLocalBrowser(Side))
  {
    TInstantOperationVisualizer Visualizer;
    dynamic_cast<TDirView *>(DirView(Side))->CopyToClipBoard(OnFocused);
  }
  else
  {
    TCustomScpExplorerForm::CopyFilesToClipboard(Side, OnFocused);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::PasteFromClipBoard()
{
  if (DoesClipboardContainOurFiles() && (IsSideLocalBrowser(osCurrent)))
  {
    if (DebugAlwaysTrue(CanPasteToDirViewFromClipBoard()))
    {
      ClipboardDownload(DirView(osCurrent)->Path, !WinConfiguration->ConfirmTransferring, false);
    }
  }
  else
  {
    TCustomScpExplorerForm::PasteFromClipBoard();
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::FileColorsChanged()
{
  TCustomScpExplorerForm::FileColorsChanged();
  DoFileColorsChanged(LocalDirView);
  DoFileColorsChanged(OtherLocalDirView);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::BrowseFile(const UnicodeString & FileName)
{
  DebugAssert(!IsLocalBrowserMode());
  TCustomScpExplorerForm::BrowseFile(FileName);
  DoBrowseFile(LocalDirView, FileName);
  TScpCommanderConfiguration ScpCommander = WinConfiguration->ScpCommander;
  // Select the panel that has the file, with preference on the remote panel
  if ((RemoteDirView->ItemFocused != NULL) && RemoteDirView->ItemFocused->Selected)
  {
    ScpCommander.CurrentPanel = osRemote;
  }
  else if ((LocalDirView->ItemFocused != NULL) && LocalDirView->ItemFocused->Selected)
  {
    ScpCommander.CurrentPanel = osLocal;
  }
  WinConfiguration->ScpCommander = ScpCommander;
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::ThemeChanged()
{
  TCustomScpExplorerForm::ThemeChanged();
  LocalDirView->Perform(WM_THEMECHANGED, 0, 0);
  OtherLocalDirView->Perform(WM_THEMECHANGED, 0, 0);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::OtherLocalDirViewEnter(TObject *)
{
  // Actually identical to LocalDirViewEnter
  SideEnter(osOther);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::OtherLocalDriveViewEnter(TObject *)
{
  MakeNextInTabOrder(OtherLocalDirView, OtherLocalDriveView);
  SideEnter(osOther);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::DoRemotePathComboBoxAdjustImageIndex(
  TTBXComboBoxItem * Sender, const UnicodeString AText, int AIndex, int & ImageIndex)
{
  if (!IsLocalBrowserMode())
  {
    TCustomScpExplorerForm::DoRemotePathComboBoxAdjustImageIndex(Sender, AText, AIndex, ImageIndex);
  }
  else
  {
    DoLocalPathComboBoxAdjustImageIndex(Sender, AText, AIndex, ImageIndex);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::DoRemotePathComboBoxCancel(TObject * Sender)
{
  if (!IsLocalBrowserMode())
  {
    TCustomScpExplorerForm::DoRemotePathComboBoxCancel(Sender);
  }
  else
  {
    LocalPathComboUpdate(OtherLocalDirView, RemotePathComboBox);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::OtherLocalDirViewPathChange(TCustomDirView * Sender)
{
  // should happen only when called from TScpCommanderForm::DoShow while starting connected
  if (IsLocalBrowserMode())
  {
    DebugUsedParam(Sender);
    DebugAssert(Sender == OtherLocalDirView);
    UpdateRemotePathComboBox(!FSessionChanging);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::DoRemotePathComboBoxItemClick(TObject * Sender)
{
  if (!IsLocalBrowserMode())
  {
    TCustomScpExplorerForm::DoRemotePathComboBoxItemClick(Sender);
  }
  else
  {
    DoLocalPathComboBoxItemClick(OtherLocalDirView, RemotePathComboBox);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::UpdateRemotePathComboBox(bool TextOnly)
{
  if (!IsLocalBrowserMode())
  {
    TCustomScpExplorerForm::UpdateRemotePathComboBox(TextOnly);
  }
  else
  {
    if (!TextOnly)
    {
      RemotePathComboBox->Strings->Assign(LocalPathComboBox->Strings);
    }
    LocalPathComboUpdate(OtherLocalDirView, RemotePathComboBox);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::LocalDriveViewNeedHiddenDirectories(TObject *)
{
  if (DebugAlwaysTrue(!WinConfiguration->ShowHiddenFiles))
  {
    ToggleShowHiddenFiles();
  }
}
//---------------------------------------------------------------------------
void TScpCommanderForm::LocalLocalCopy(
  ::TFileOperation Operation, TOperationSide Side, bool OnFocused, bool NoConfirmation, bool DragDrop, unsigned int Flags)
{
  std::unique_ptr<TFileOperator> FileOperator(new TFileOperator(NULL));
  bool Move;
  switch (Operation)
  {
    case ::foCopy:
      FileOperator->Operation = Fileoperator::foCopy;
      Move = false;
      break;
    case ::foMove:
      FileOperator->Operation = Fileoperator::foMove;
      Move = true;
      break;
    default:
      DebugFail();
      Abort();
  }

  TOperationSide OtherSide = GetOtherSide(GetSide(Side));

  TCustomDirView * SourceDirView = DirView(Side);
  UnicodeString DestinationDir = DirView(OtherSide)->PathName;
  SourceDirView->CreateFileList(OnFocused, true, FileOperator->OperandFrom);

  int OutputOptions =
    FLAGMASK(GetDoNotShowCopyDialogDefault(DragDrop), clooDoNotShowAgain);
  bool MultipleFiles = (FileOperator->OperandFrom->Count > 1);
  int Options =
    FLAGMASK(FLAGSET(Flags, cocShortCutHint), cloShortCutHint) |
    FLAGMASK(MultipleFiles, cloMultipleFiles);
  UnicodeString FileMask = AnyMask;

  bool Confirmed =
    NoConfirmation ||
    DoCopyLocalDialog(Move, Options, DestinationDir, FileMask, OutputOptions);

  if (Confirmed)
  {
    HandleDoNotShowCopyDialogAgain(DragDrop, FLAGSET(OutputOptions, clooDoNotShowAgain));

    FileOperator->Flags = FileOperator->Flags << foMultiDestFiles;
    if (!WinConfiguration->ConfirmOverwriting)
    {
      FileOperator->Flags = FileOperator->Flags << foNoConfirmation;
    }

    if (Move)
    {
      Configuration->Usage->Inc(L"LocalLocalMovesCommand");
    }
    else
    {
      Configuration->Usage->Inc(L"LocalLocalCopiesCommand");
    }

    for (int Index = 0; Index < FileOperator->OperandFrom->Count; Index++)
    {
      UnicodeString SourcePath = FileOperator->OperandFrom->Strings[Index];
      UnicodeString FileName = TPath::GetFileName(SourcePath);
      FileName = MaskFileName(FileName, FileMask);
      UnicodeString DestinationPath = CombinePaths(DestinationDir, FileName);
      FileOperator->OperandTo->Add(DestinationPath);
    }

    SourceDirView->ClearSelection();

    {
      TAutoBatch AutoBatch(this);
      FileOperator->Execute();
    }

    ReloadLocalDirectory(DestinationDir);
    if (Operation == ::foMove)
    {
      UnicodeString SourceDir = SourceDirView->PathName;
      if (!SamePaths(SourceDir, DestinationDir))
      {
        ReloadLocalDirectory(SourceDir);
      }
    }
  }
}
//---------------------------------------------------------------------------
UnicodeString TScpCommanderForm::GetLocalBrowserSessionTitle(TManagedTerminal * ASession)
{
  UnicodeString Result;
  if (ASession->SessionData->HasSessionName())
  {
    Result = ASession->SessionData->SessionName;
  }
  else
  {
    DebugAssert(ASession->LocalBrowser);
    UnicodeString Path1;
    UnicodeString Path2;
    TTerminalManager * Manager = TTerminalManager::Instance();
    // might use GetSessionPath here
    if ((ASession == ManagedSession) &&
        // prevent tab title flicker, when switching to local-local tab, as the path changes in individual local panels
        !FSessionChanging)
    {
      Path1 = LocalDirView->PathName;
      Path2 = OtherLocalDirView->PathName;
    }
    else
    {
      Path1 = ASession->StateData->LocalDirectory;
      Path2 = ASession->StateData->OtherLocalDirectory;
    }
    // See also TSessionData::GetDefaultSessionName()
    Path1 = Manager->GetPathForSessionTabName(ExtractShortName(Path1, false));
    Path2 = Manager->GetPathForSessionTabName(ExtractShortName(Path2, false));
    Result = Path1 + TitleSeparator + Path2;
  }
  return Result;
}
//---------------------------------------------------------------------------
int TScpCommanderForm::GetNewTabActionImageIndex()
{
  return WinConfiguration->DefaultToNewRemoteTab ?
    TCustomScpExplorerForm::GetNewTabActionImageIndex() : NonVisualDataModule->NewLocalTabAction->ImageIndex;
}
//---------------------------------------------------------------------------
int TScpCommanderForm::GetNewTabTabImageIndex(TOperationSide Side)
{
  if (Side == osCurrent)
  {
    Side = WinConfiguration->DefaultToNewRemoteTab ? osRemote : osLocal;
  }
  return TCustomScpExplorerForm::GetNewTabTabImageIndex(Side);
}
//---------------------------------------------------------------------------
UnicodeString TScpCommanderForm::GetTabHintDetails(TManagedTerminal * ASession)
{
  UnicodeString Result;
  if (!ASession->LocalBrowser)
  {
    Result = GetTabHintSessionDetails(ASession);
  }
  if (!ASession->LocalBrowser && !ASession->Active)
  {
    Result += LoadStr(STATUS_NOT_CONNECTED2);
  }
  else
  {
    UnicodeString Local = GetSessionPath(ASession, osLocal);
    UnicodeString Other = GetSessionPath(ASession, osOther);
    // Contrary to Panel(), the IsRightToLeft() is not considered here,
    // as I assume they expect the right panel first, as they read from the right.
    UnicodeString Sep = L"\n";
    if (!WinConfiguration->ScpCommander.SwappedPanels)
    {
      Result += Local + Sep + Other;
    }
    else
    {
      Result += Other + Sep + Local;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString TScpCommanderForm::GetNewTabHintDetails()
{
  UnicodeString Result;
  if (WinConfiguration->DefaultToNewRemoteTab)
  {
    Result = TCustomScpExplorerForm::GetNewTabHintDetails();
    Result = FMTLOAD(NEW_REMOTE_TAB_CTRL_HINT, (Result));
  }
  else
  {
    Result = LoadStr(NEW_LOCAL_TAB_HINT);
  }
  return Result;
}
//---------------------------------------------------------------------------
bool TScpCommanderForm::SupportedSession(TSessionData *)
{
  return true;
}
//---------------------------------------------------------------------------
void TScpCommanderForm::ResetLayoutColumns(TOperationSide Side)
{
  UnicodeString DirViewParamsDefault =
    IsSideLocalBrowser(Side) ? ScpCommanderLocalPanelDirViewParamsDefault : ScpCommanderRemotePanelDirViewParamsDefault;

  DirView(Side)->ColProperties->ParamsStr = DirViewParamsDefault;
}
//---------------------------------------------------------------------------
void * TScpCommanderForm::SaveFocus()
{
  // When window is disabled, the ActiveControl might actually not be focusable
  return ((ActiveControl != NULL) && ActiveControl->Focused()) ? ActiveControl : NULL;
}
//---------------------------------------------------------------------------
void TScpCommanderForm::RestoreFocus(void * Focus)
{
  TWinControl * ControlFocus = static_cast<TWinControl *>(Focus);
  if ((ControlFocus == LocalDirView) || (ControlFocus == LocalDriveView))
  {
    DebugAssert(ActiveControl == ControlFocus);
    DebugAssert(ControlFocus->CanFocus());
    // noop
  }
  else if ((ControlFocus == RemoteDirView) || (ControlFocus == OtherLocalDirView))
  {
    TCustomDirView * OtherDirView = DirView(osOther);
    if (OtherDirView->CanFocus())
    {
      ControlFocus = OtherDirView;
    }
    else
    {
      DebugAssert(LocalDirView->CanFocus());
      ControlFocus = LocalDirView;
    }
  }
  else if ((ControlFocus == RemoteDriveView) || (ControlFocus == OtherLocalDriveView))
  {
    TCustomDriveView * OtherDriveView = DriveView(osOther);
    if (OtherDriveView->CanFocus())
    {
      ControlFocus = OtherDriveView;
    }
    else if (LocalDriveView->CanFocus())
    {
      ControlFocus = LocalDriveView;
    }
    else
    {
      // Switching to a disconnected session with local drive view hidden -
      // fallback to the local dir view (should always be visible)
      DebugAssert(LocalDirView->CanFocus());
      ControlFocus = LocalDirView;
    }
  }
  else
  {
    ControlFocus = NULL;
  }

  // The CanFocus check is hack and should be removed eventually
  if ((ControlFocus != NULL) && DebugAlwaysTrue(ControlFocus->CanFocus()))
  {
    ActiveControl = ControlFocus;
  }
}
