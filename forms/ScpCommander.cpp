//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "ScpCommander.h"

#include <Common.h>
#include <Net.h>
#include <ScpMain.h>
#include <Interface.h>
#include <TextsWin.h>
#include <VCLCommon.h>
#include <GUITools.h>
#include <DragDrop.hpp>

#include "NonVisual.h"
#include "Tools.h"
#include "WinConfiguration.h"
#include "TerminalManager.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AssociatedStatusBar"
#pragma link "CustomDirView"
#pragma link "CustomScpExplorer"
#pragma link "CustomUnixDirView"
#pragma link "IEListView"
#pragma link "NortonLikeListView"
#pragma link "UnixDirView"
#pragma link "DirView"
#pragma link "CustomPathComboBox"
#pragma link "IEComboBox"
#pragma link "IEPathComboBox"
#pragma link "PathLabel"
#pragma link "UnixPathComboBox"
#pragma link "ToolbarPanel"
#pragma link "HistoryComboBox"
#pragma link "CustomDriveView"
#pragma link "DriveView"
#pragma link "UnixDriveView"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
__fastcall TScpCommanderForm::TScpCommanderForm(TComponent* Owner)
        : TCustomScpExplorerForm(Owner)
{
  FCurrentSide = osLocal;
  FLastLocalPanelWidth = LocalPanelWidth;
  FSynchronisingBrowse = false;
  FFirstTerminal = true;
  FInternalDDDownloadList = new TStringList();

  LocalBackButton->DropdownMenu = LocalDirView->BackMenu;
  LocalForwardButton->DropdownMenu = LocalDirView->ForwardMenu;
  RemoteBackButton->DropdownMenu = RemoteDirView->BackMenu;
  RemoteForwardButton->DropdownMenu = RemoteDirView->ForwardMenu;
  SavedSessionsButton->OnClick = DropDownButtonMenu;

  TopCoolBar->PopupMenu = NonVisualDataModule->CommanderBarPopup;
  ToolbarPanel->PopupMenu = TopCoolBar->PopupMenu;
  StatusBar->PopupMenu = TopCoolBar->PopupMenu;
  QueueCoolBar->PopupMenu = TopCoolBar->PopupMenu;

  LocalCoolBar->PopupMenu = NonVisualDataModule->LocalPanelPopup;
  LocalPathLabel->PopupMenu = LocalCoolBar->PopupMenu;
  LocalStatusBar->PopupMenu = LocalCoolBar->PopupMenu;
  LocalDriveView->PopupMenu = LocalCoolBar->PopupMenu;

  RemoteCoolBar->PopupMenu = NonVisualDataModule->RemotePanelPopup;
  RemotePathLabel->PopupMenu = RemoteCoolBar->PopupMenu;
  RemoteStatusBar->PopupMenu = RemoteCoolBar->PopupMenu;
  RemoteDriveView->PopupMenu = RemoteCoolBar->PopupMenu;

  // set common norton shorcuts to our actions
  NonVisualDataModule->CommanderShortcuts();
  Splitter->ShowHint = True;
  reinterpret_cast<TLabel*>(Splitter)->OnDblClick = SplitterDblClick;
  reinterpret_cast<TLabel*>(LocalPanelSplitter)->OnDblClick = PanelSplitterDblClick;
  reinterpret_cast<TLabel*>(RemotePanelSplitter)->OnDblClick = PanelSplitterDblClick;
  RemotePathComboBox->TabStop = False;

  CommandLineLabel->FocusControl = CommandLineCombo;
  CommandLineCombo->Text = "";
  FCommandLineComboPopulated = false;

  LocalDirView->Font = Screen->IconFont;
}
//---------------------------------------------------------------------------
__fastcall TScpCommanderForm::~TScpCommanderForm()
{
  delete FInternalDDDownloadList;
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::RestoreFormParams()
{
  assert(WinConfiguration);
  TCustomScpExplorerForm::RestoreFormParams();
  WinConfiguration->RestoreForm(WinConfiguration->ScpCommander.WindowParams, this);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::RestoreParams()
{
  assert(Configuration);

  // called later once again after menu font is updated (see FormShow)
  SetCoolBandsMinWidth(TopCoolBar);
  SetCoolBandsMinWidth(LocalCoolBar);
  SetCoolBandsMinWidth(RemoteCoolBar);

  // IDE often looses this link
  LocalDirView->HeaderImages = NonVisualDataModule->ArrowImages;

  TCustomScpExplorerForm::RestoreParams();
  LocalPanelWidth = WinConfiguration->ScpCommander.LocalPanelWidth;
  LoadCoolbarLayoutStr(TopCoolBar, WinConfiguration->ScpCommander.CoolBarLayout);
  StatusBar->Visible = WinConfiguration->ScpCommander.StatusBar;
  ToolbarPanel->Visible = WinConfiguration->ScpCommander.ToolBar;

  CommandLinePanel->Visible = WinConfiguration->ScpCommander.CommandLine;

  FDirViewToSelect = (WinConfiguration->ScpCommander.CurrentPanel == osLocal ?
    (TCustomDirView *)LocalDirView : (TCustomDirView *)RemoteDirView);
  #define RESTORE_PANEL_PARAMS(PANEL) \
    PANEL ## DirView->ColProperties->ParamsStr = WinConfiguration->ScpCommander.PANEL ## Panel.DirViewParams; \
    PANEL ## StatusBar->Visible = WinConfiguration->ScpCommander.PANEL ## Panel.StatusBar; \
    LoadCoolbarLayoutStr(PANEL ## CoolBar, WinConfiguration->ScpCommander.PANEL ## Panel.CoolBarLayout); \
    PANEL ## DriveView->Visible = WinConfiguration->ScpCommander.PANEL ## Panel.DriveView; \
    PANEL ## DriveView->Height = WinConfiguration->ScpCommander.PANEL ## Panel.DriveViewHeight
  RESTORE_PANEL_PARAMS(Local);
  RESTORE_PANEL_PARAMS(Remote);
  #undef RESTORE_PANEL_PARAMS

  NonVisualDataModule->SynchronizeBrowsingAction->Checked = WinConfiguration->ScpCommander.SynchronizeBrowsing;
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::StoreParams()
{
  assert(WinConfiguration);

  WinConfiguration->BeginUpdate();
  try
  {
    WinConfiguration->ScpCommander.CoolBarLayout = GetCoolbarLayoutStr(TopCoolBar);
    WinConfiguration->ScpCommander.LocalPanelWidth = LocalPanelWidth;
    WinConfiguration->ScpCommander.StatusBar = StatusBar->Visible;
    WinConfiguration->ScpCommander.ToolBar = ToolbarPanel->Visible;

    WinConfiguration->ScpCommander.CommandLine = CommandLinePanel->Visible;
    SaveCommandLine();

    WinConfiguration->ScpCommander.CurrentPanel = FCurrentSide;

    #define STORE_PANEL_PARAMS(PANEL) \
      WinConfiguration->ScpCommander.PANEL ## Panel.DirViewParams = PANEL ## DirView->ColProperties->ParamsStr; \
      WinConfiguration->ScpCommander.PANEL ## Panel.StatusBar = PANEL ## StatusBar->Visible; \
      WinConfiguration->ScpCommander.PANEL ## Panel.CoolBarLayout = GetCoolbarLayoutStr(PANEL ## CoolBar); \
      WinConfiguration->ScpCommander.PANEL ## Panel.DriveView = PANEL ## DriveView->Visible; \
      WinConfiguration->ScpCommander.PANEL ## Panel.DriveViewHeight = PANEL ## DriveView->Height 
    STORE_PANEL_PARAMS(Local);
    STORE_PANEL_PARAMS(Remote);
    #undef RESTORE_PANEL_PARAMS

    WinConfiguration->ScpCommander.WindowParams = WinConfiguration->StoreForm(this);;

    WinConfiguration->ScpCommander.SynchronizeBrowsing = NonVisualDataModule->SynchronizeBrowsingAction->Checked;

    TCustomScpExplorerForm::StoreParams();
  }
  __finally
  {
    WinConfiguration->EndUpdate();
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::UpdateSessionData(TSessionData * Data)
{
  assert(Terminal && Terminal->SessionData);

  if (!Data)
  {
    Data = Terminal->SessionData;
  }
  TCustomScpExplorerForm::UpdateSessionData(Data);

  assert(LocalDirView);
  Data->LocalDirectory = LocalDirView->PathName;
}
//---------------------------------------------------------------------------
bool __fastcall TScpCommanderForm::InternalDDDownload(AnsiString & TargetDirectory)
{
  assert(IsFileControl(FDDTargetControl, osLocal));
  
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
      TargetDirectory = IncludeTrailingBackslash(LocalDirView->Path);
      Result = true;
    }
  }
  else if (FDDTargetControl == LocalDriveView)
  {
    assert(LocalDriveView->DropTarget != NULL);
    TargetDirectory = LocalDriveView->NodePathName(LocalDriveView->DropTarget);
    Result = true;
  }
  else
  {
    assert(false);
    Abort();
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TScpCommanderForm::CopyParamDialog(TTransferDirection Direction,
  TTransferType Type, bool DragDrop, TStrings * FileList, AnsiString & TargetDirectory,
  TGUICopyParamType & CopyParam, bool Confirm)
{
  bool Result = false;
  if (DragDrop && (Direction == tdToLocal) &&
      IsFileControl(FDDTargetControl, osLocal))
  {
    Result = InternalDDDownload(TargetDirectory);
    if (Result)
    {
      assert(FileList->Count > 0);
      FInternalDDDownloadList->Assign(FileList);
    }
  }
  else if (!DragDrop && TargetDirectory.IsEmpty())
  {
    if (Direction == tdToLocal)
    {
      TargetDirectory = IncludeTrailingBackslash(LocalDirView->Path);
    }
    else
    {
      TargetDirectory = UnixIncludeTrailingBackslash(RemoteDirView->Path);
    }
  }

  if (!Result)
  {
    Result = TCustomScpExplorerForm::CopyParamDialog(Direction, Type, DragDrop,
      FileList, TargetDirectory, CopyParam, Confirm);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::DoShow()
{
  TCustomScpExplorerForm::DoShow();

  assert(FDirViewToSelect);
  FDirViewToSelect->SetFocus();

  // called for second time after menu font was updated (see also RestoreParams)
  SetCoolBandsMinWidth(TopCoolBar);
  SetCoolBandsMinWidth(LocalCoolBar);
  SetCoolBandsMinWidth(RemoteCoolBar);

  UpdateControls();
}
//---------------------------------------------------------------------------
Boolean __fastcall TScpCommanderForm::AllowedAction(TAction * Action, TActionAllowed Allowed)
{
  #define FLAG ((TActionFlag)(Action->Tag))
  return
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
  else
  {
    return TCustomScpExplorerForm::DirView(Side);
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
void __fastcall TScpCommanderForm::ReloadLocalDirectory(const AnsiString Directory)
{
  if (Directory.IsEmpty() || ComparePaths(Directory, LocalDirView->Path))
  {
    LocalDirView->ReloadDirectory();
    LocalDriveView->ValidateDirectory(LocalDriveView->Selected);
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

  TCustomScpExplorerForm::BatchStart(Storage);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::BatchEnd(void * Storage)
{
  TCustomScpExplorerForm::BatchEnd(Storage);

  assert(Storage != NULL);

  LocalDirView->WatchForChanges = *static_cast<bool*>(Storage);
  LocalDriveView->WatchDirectory = LocalDirView->WatchForChanges;

  delete Storage;
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::TerminalChanged()
{
  if (Terminal)
  {
    bool WasSynchronisingBrowsing = NonVisualDataModule->SynchronizeBrowsingAction->Checked;
    NonVisualDataModule->SynchronizeBrowsingAction->Checked = false;

    TCustomScpExplorerForm::TerminalChanged();

    if (FFirstTerminal || !WinConfiguration->ScpCommander.PreserveLocalDirectory)
    {
      AnsiString LocalDirectory = Terminal->SessionData->LocalDirectory;
      bool DocumentsDir = LocalDirectory.IsEmpty();

      if (!DocumentsDir)
      {
        try
        {
          LocalDirView->Path = LocalDirectory;
        }
        catch(Exception & E)
        {
          DocumentsDir = true;
          Terminal->DoShowExtendedException(&E);
        }
      }

      if (DocumentsDir)
      {
        try
        {
          LocalDirView->HomeDirectory = "";
          LocalDirView->ExecuteHomeDirectory();
        }
        catch(Exception & E)
        {
          Terminal->DoShowExtendedException(&E);
          LocalDirView->Path = ExtractFilePath(Application->ExeName);
        }
      }

      if (WinConfiguration->DefaultDirIsHome &&
          !Terminal->SessionData->UpdateDirectories)
      {
        LocalDirView->HomeDirectory = LocalDirectory;
      }
    }
    FFirstTerminal = false;

    if (WasSynchronisingBrowsing &&
        SameText(ExtractFileName(LocalDirView->PathName),
          UnixExtractFileName(RemoteDirView->PathName)))
    {
      NonVisualDataModule->SynchronizeBrowsingAction->Checked = true;
    }
  }
  else
  {
    TCustomScpExplorerForm::TerminalChanged();
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::ConfigurationChanged()
{
  TCustomScpExplorerForm::ConfigurationChanged();
  if (WinConfiguration->DefaultDirIsHome && Terminal)
  {
    LocalDirView->HomeDirectory = Terminal->SessionData->LocalDirectory;
  }
  else
  {
    LocalDirView->HomeDirectory = "";
  }
  LocalDirView->DimmHiddenFiles = WinConfiguration->DimmHiddenFiles;
  LocalDriveView->DimmHiddenDirs = WinConfiguration->DimmHiddenFiles;
  LocalDirView->ShowHiddenFiles = WinConfiguration->ShowHiddenFiles;
  LocalDriveView->ShowHiddenDirs = WinConfiguration->ShowHiddenFiles;

  LocalDirView->NortonLike = !WinConfiguration->ScpCommander.ExplorerStyleSelection;
  RemoteDirView->NortonLike = !WinConfiguration->ScpCommander.ExplorerStyleSelection;

  LocalDirView->DragDropFilesEx->ShellExtensions->DropHandler =
    !WinConfiguration->DDExtEnabled;
  LocalDriveView->DragDropFilesEx->ShellExtensions->DropHandler =
    !WinConfiguration->DDExtEnabled;
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::SetLocalPanelWidth(float value)
{
  float Total = LocalPanel->Width + RemotePanel->Width;
  FLocalPanelWidth = value;
  if (value * Total != LocalPanel->Width)
  {
    LocalPanel->Width = value * Total;
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
float __fastcall TScpCommanderForm::GetLocalPanelWidth()
{
  return FLocalPanelWidth;
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::SplitterMoved(TObject * /*Sender*/)
{
  float Local = LocalPanel->Width;
  float Total = LocalPanel->Width + RemotePanel->Width;
  FLocalPanelWidth = Local / Total;
  FLastLocalPanelWidth = LocalPanelWidth;
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::SplitterCanResize(TObject * /*Sender*/,
      int &NewSize, bool & /*Accept*/)
{
  // When splitter is drag so far to right, that width contraint of remote panel would
  // be violated, it doesn't stop, but extend form width.
  // Following prevents this behaviour.
  if (ClientWidth - NewSize - Splitter->Width < RemotePanel->Constraints->MinWidth)
    NewSize = (ClientWidth - RemotePanel->Constraints->MinWidth - Splitter->Width);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::SplitterDblClick(TObject * /*Sender*/)
{
  LocalPanelWidth = 0.5;
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::PanelSplitterDblClick(TObject * Sender)
{
  TSplitter * Splitter = dynamic_cast<TSplitter *>(Sender);
  assert(Splitter != NULL);
  TCustomDriveView * DriveView;
  TCustomDriveView * OtherDriveView;
  if (Splitter == LocalPanelSplitter)
  {
    DriveView = LocalDriveView;
    OtherDriveView = RemoteDriveView;
  }
  else
  {
    DriveView = RemoteDriveView;
    OtherDriveView = LocalDriveView;
  }

  assert(DriveView->Visible);
  if (OtherDriveView->Visible)
  {
    DriveView->Height = OtherDriveView->Height;
  }
  else
  {
    OtherDriveView->Height = DriveView->Height;
    OtherDriveView->Visible = true;
  }

  FixControlsPlacement();
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::UpdateControls()
{
  Splitter->Hint = FormatFloat("0%|X", LocalPanelWidth*100);
  CommandLineLabel->UnixPath = (FCurrentSide == osRemote);
  CommandLineLabel->Caption = DirView(osCurrent)->PathName;
  CommandLinePromptLabel->Caption =
    (FCurrentSide == osRemote) ? "$" : ">";
  EnableControl(CommandLineCombo,
    (FCurrentSide == osLocal) ||
    ((Terminal != NULL) && Terminal->IsCapable[fcAnyCommand]));
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::ChangePath(TOperationSide Side)
{
  assert((Side == osLocal) || (Side == osRemote));
  TCustomPathComboBox * PathComboBox;
  if (Side == osLocal) PathComboBox = LocalPathComboBox;
    else PathComboBox = RemotePathComboBox;
  assert(PathComboBox);
  PathComboBox->SetFocus();
  PathComboBox->DroppedDown = True;
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::PathComboBoxCloseUp(TObject * /*Sender*/,
      bool /*Canceled*/)
{
  DirView(osCurrent)->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::SessionComboCloseUp(TObject *Sender)
{
  PathComboBoxCloseUp(Sender, false);
}
//---------------------------------------------------------------------------
TControl * __fastcall TScpCommanderForm::GetComponent(Byte Component)
{
  switch (Component) {
    case fcToolBar: return ToolbarPanel;
    case fcStatusBar: return StatusBar;
    case fcLocalCoolBar: return LocalCoolBar;
    case fcLocalStatusBar: return LocalStatusBar;
    case fcRemoteCoolBar: return RemoteCoolBar;
    case fcRemoteStatusBar: return RemoteStatusBar;
    case fcSessionCombo: return SessionCombo;
    case fcMenuToolBar: return MenuToolBar;
    case fcCommandLinePanel: return CommandLinePanel; 
    case fcLocalTree: return LocalDriveView;
    default: return TCustomScpExplorerForm::GetComponent(Component);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::FixControlsPlacement()
{
  TCustomScpExplorerForm::FixControlsPlacement();

  LocalPanelSplitter->Visible = LocalDriveView->Visible;

  TControl * ControlsOrder[] =
    { CommandLinePanel, QueueSplitter, QueuePanel, ToolbarPanel, StatusBar };
  SetVerticalControlsOrder(ControlsOrder, LENOF(ControlsOrder));

  TControl * LocalControlsOrder[] =
    { LocalDriveView, LocalPanelSplitter, LocalDirView };
  SetVerticalControlsOrder(LocalControlsOrder, LENOF(LocalControlsOrder));

  TControl * RemoteControlsOrder[] =
    { RemoteDriveView, RemotePanelSplitter, RemoteDirView };
  SetVerticalControlsOrder(RemoteControlsOrder, LENOF(RemoteControlsOrder));

  if (LocalDirView->ItemFocused != NULL)
  {
    LocalDirView->ItemFocused->MakeVisible(false);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TScpCommanderForm::GetHasDirView(TOperationSide Side)
{
  return TCustomScpExplorerForm::GetHasDirView(Side) || (Side == osLocal);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::CompareDirectories()
{
  LocalDirView->CompareFiles(RemoteDirView, false,
    WinConfiguration->ScpCommander.CompareCriterias());
  RemoteDirView->CompareFiles(LocalDirView, false,
    WinConfiguration->ScpCommander.CompareCriterias());
  if (LocalDirView->SelCount + RemoteDirView->SelCount == 0)
  {
    MessageDialog(LoadStr(COMPARE_NO_DIFFERENCES), qtInformation, qaOK, 0);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::SynchronizeDirectories()
{
  AnsiString LocalDirectory = LocalDirView->PathName;
  AnsiString RemoteDirectory = RemoteDirView->PathName;
  DoSynchronizeDirectories(LocalDirectory, RemoteDirectory);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::FullSynchronizeDirectories()
{
  AnsiString LocalDirectory = LocalDirView->PathName;
  AnsiString RemoteDirectory = RemoteDirView->PathName;
  TSynchronizeMode Mode = (FCurrentSide == osLocal) ? smRemote : smLocal;
  DoFullSynchronizeDirectories(LocalDirectory, RemoteDirectory, Mode);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::ExploreLocalDirectory()
{
  if ((int)ShellExecute(Application->Handle, "explore",
      (char*)LocalDirView->Path.data(), NULL, NULL, SW_SHOWNORMAL) <= 32)
  {
    throw Exception(FORMAT(EXPLORE_LOCAL_DIR_ERROR, (LocalDirView->Path)));
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::LocalDirViewExecFile(TObject *Sender,
      TListItem *Item, bool &AllowExec)
{
  assert(Item);
  if ((UpperCase(PFileRec(Item->Data)->FileExt) == "LNK") &&
      DirectoryExists(ResolveFileShortCut(LocalDirView->ItemFullFileName(Item), true)))
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
  // literal type of 'DataObj' parameter.Actual type is however same
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
  TFileOperationProgressType & ProgressData, TCancelStatus & Cancel)
{
  // Heuristic: When operation finishes and DD targed is local dir view,
  // we suppose that drag&drop download finished, so local dir view should be
  // reloaded
  if (!ProgressData.InProgress && FProgressForm &&
      IsFileControl(FDDTargetControl, osLocal) &&
      ((ProgressData.Operation == ::foCopy) || (ProgressData.Operation == ::foMove)))
  {
    ReloadLocalDirectory();
  }
  TCustomScpExplorerForm::FileOperationProgress(ProgressData, Cancel);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::DirViewLoaded(TObject *Sender)
{
  UpdateControls();
  
  try
  {
    TCustomDirView * ADirView = dynamic_cast<TCustomDirView *>(Sender);
    assert(ADirView);
    AnsiString PrevPath = FPrevPath[ADirView == LocalDirView];
    AnsiString FullPrevPath =
      (ADirView == LocalDirView) ? IncludeTrailingBackslash(PrevPath) :
        UnixIncludeTrailingBackslash(PrevPath);
    FPrevPath[ADirView == LocalDirView] = ADirView->Path;

    if (!FSynchronisingBrowse && NonVisualDataModule->SynchronizeBrowsingAction->Checked &&
        !PrevPath.IsEmpty() && PrevPath != ADirView->Path)
    {
      FSynchronisingBrowse = true;
      if (ADirView == LocalDirView)
      {
        Terminal->ExceptionOnFail = true;
        try
        {
          if (LocalDirView->Path.SubString(1, FullPrevPath.Length()) == FullPrevPath)
          {
            RemoteDirView->Path = UnixIncludeTrailingBackslash(RemoteDirView->Path) +
              ToUnixPath(LocalDirView->Path.SubString(FullPrevPath.Length() + 1,
                LocalDirView->Path.Length() - FullPrevPath.Length()));
          }
          else if (FullPrevPath.SubString(1, LocalDirView->Path.Length()) == LocalDirView->Path)
          {
            AnsiString RemotePath = RemoteDirView->Path;
            while (!ComparePaths(FullPrevPath, LocalDirView->Path))
            {
              if (RemotePath == UnixExcludeTrailingBackslash(RemotePath))
              {
                Abort();
              }
              RemotePath = UnixExtractFilePath(UnixExcludeTrailingBackslash(RemotePath));
              FullPrevPath = ExtractFilePath(ExcludeTrailingBackslash(FullPrevPath));
            }
            RemoteDirView->Path = RemotePath;
          }
          else
          {
            Abort();
          }
        }
        __finally
        {
          Terminal->ExceptionOnFail = false;
        }
      }
      else
      {
        if (RemoteDirView->Path.SubString(1, FullPrevPath.Length()) == FullPrevPath)
        {
          LocalDirView->Path = IncludeTrailingBackslash(LocalDirView->Path) +
            FromUnixPath(RemoteDirView->Path.SubString(FullPrevPath.Length() + 1,
              RemoteDirView->Path.Length() - FullPrevPath.Length()));
        }
        else if (FullPrevPath.SubString(1, RemoteDirView->Path.Length()) == RemoteDirView->Path)
        {
          AnsiString NewLocalPath;
          AnsiString LocalPath = ExcludeTrailingBackslash(LocalDirView->Path);
          while (!UnixComparePaths(FullPrevPath, RemoteDirView->Path))
          {
            NewLocalPath = ExcludeTrailingBackslash(ExtractFileDir(LocalPath));
            if (NewLocalPath == LocalPath)
            {
              Abort();
            }
            LocalPath = NewLocalPath;
            FullPrevPath = UnixExtractFilePath(UnixExcludeTrailingBackslash(FullPrevPath));
          }
          LocalDirView->Path = LocalPath;
        }
        else
        {
          Abort();
        }
      }
      FSynchronisingBrowse = false;
    }
  }
  catch(Exception & E)
  {
    FSynchronisingBrowse = false;
    NonVisualDataModule->SynchronizeBrowsingAction->Checked = false;
    if (!Application->Terminated)
    {
      Terminal->DoShowExtendedException(&E);
      MessageDialog(LoadStr(SYNC_DIR_BROWSE_ERROR), qtInformation, qaOK, 0);
    }
    else
    {
      throw;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::AddEditLink()
{
  if (FCurrentSide == osLocal)
  {
    bool Edit = false;
    AnsiString FileName;
    AnsiString PointTo;
    bool SymbolicLink = true;

    if (LocalDirView->ItemFocused)
    {
      assert(LocalDirView->ItemFocused->Data);
      PFileRec FileRec = (PFileRec)LocalDirView->ItemFocused->Data;

      Edit = UpperCase(FileRec->FileExt) == "LNK";
      if (Edit)
      {
        AnsiString FullName = LocalDirView->ItemFullFileName(LocalDirView->ItemFocused);
        FileName = FullName;//FileRec->FileName;
        PointTo = ResolveFileShortCut(FullName, false);
        if (PointTo.IsEmpty())
        {
          throw Exception(FMTLOAD(RESOLVE_SHORTCUT_ERROR, (FullName)));
        }
      }
      else
      {
        PointTo = FileRec->FileName;
      }
    }

    if (DoSymlinkDialog(FileName, PointTo, osLocal, SymbolicLink, Edit, false))
    {
      assert(SymbolicLink);
      assert(!FileName.IsEmpty());
      assert(!PointTo.IsEmpty());

      if (ExtractFileDrive(FileName) == "" && FileName[1] != '\\')
      {
        FileName = IncludeTrailingBackslash(LocalDirView->PathName) + FileName;
      }
      if (ExtractFileDrive(PointTo) == "" && PointTo[1] != '\\')
      {
        PointTo = IncludeTrailingBackslash(LocalDirView->PathName) + PointTo;
      }
      if (ExtractFileExt(FileName) == "")
      {
        FileName = FileName + ".lnk";
      }

      if (Edit && !DeleteFile(FileName))
      {
        throw Exception(FMTLOAD(DELETE_LOCAL_FILE_ERROR, (FileName)));
      }
      if (!CreateFileShortCut(PointTo, FileName, ""))
      {
        throw Exception(CREATE_SHORTCUT_ERROR);
      }
    }
  }
  else
  {
    TCustomScpExplorerForm::AddEditLink();
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::DoOpenDirectoryDialog(TOpenDirectoryMode Mode,
  TOperationSide Side)
{
  if (WinConfiguration->UseLocationProfiles)
  {
    TStrings * RemoteDirectories = CreateVisitedDirectories(osRemote);
    try
    {
      AnsiString Local = LocalDirView->PathName;
      AnsiString Remote = RemoteDirView->PathName;

      if (LocationProfilesDialog(Mode, Side, Local, Remote, RemoteDirectories, Terminal))
      {
        if (!Local.IsEmpty())
        {
          LocalDirView->Path = Local;
        }
        if (!Remote.IsEmpty())
        {
          RemoteDirView->Path = Remote;
        }
      }
    }
    __finally
    {
      delete RemoteDirectories;
    }
  }
  else
  {
    TCustomScpExplorerForm::DoOpenDirectoryDialog(Mode, Side);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::LocalDirViewDDTargetHasDropHandler(
  TObject * /*Sender*/, TListItem * Item, int & /*Effect*/, bool & DropHandler)
{
  // when drop target is not directory, it is probably file type, which have
  // associated drop handler (such as ZIP file in WinXP). in this case we
  // cannot allow downloading when using shellex.
  // ! this check is duplicated in InternalDDDownload() for non-shellex downloads
  if ((FDDExtMapFile != NULL) &&
      !LocalDirView->ItemIsDirectory(Item))
  {
    DropHandler = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::LocalFileControlDDDragOver(TObject * /*Sender*/,
  int grfKeyState, TPoint & /*Point*/, int & dwEffect)
{
  if (IsFileControl(DropSourceControl, osRemote))
  {
    if (((grfKeyState & (MK_CONTROL | MK_SHIFT)) == 0) ||
        (((grfKeyState & (MK_CONTROL | MK_SHIFT)) == MK_SHIFT) &&
          !WinConfiguration->DDAllowMoveInit))
    {
      dwEffect = DROPEFFECT_Copy;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::DDGetTarget(AnsiString & Directory)
{
  if (!FDDExtTarget.IsEmpty())
  {
    Directory = FDDExtTarget;
    FDDExtTarget = "";
  }
  else
  {
    TCustomScpExplorerForm::DDGetTarget(Directory);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::DDExtInitDrag(TFileList * FileList,
  bool & Created)
{
  FDDExtTarget = "";
  TCustomScpExplorerForm::DDExtInitDrag(FileList, Created);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::LocalFileControlDDFileOperation(
  TObject * /*Sender*/, int dwEffect, AnsiString SourcePath,
  AnsiString TargetPath, bool & DoOperation)
{
  if (IsFileControl(DropSourceControl, osRemote))
  {
    AnsiString TargetDirectory;
    if (InternalDDDownload(TargetDirectory))
    {
      if (FDDExtMapFile != NULL)
      {
        FDDExtTarget = TargetDirectory;
      }
      else
      {
        assert(FInternalDDDownloadList->Count > 0);
        assert(dwEffect == DROPEFFECT_Copy || dwEffect == DROPEFFECT_Move);
        TGUICopyParamType CopyParams = GUIConfiguration->CopyParam;
        TTransferType TransferType = dwEffect == DROPEFFECT_Copy ? ttCopy : ttMove;
        if (FDDMoveSlipped)
        {
          TransferType = ttMove;
        }
        if (CopyParamDialog(tdToLocal, TransferType,
              false, FInternalDDDownloadList, TargetDirectory, CopyParams,
              WinConfiguration->DDTransferConfirmation))
        {
          int Params =
            (TransferType == ttMove ? cpDelete : 0) |
            (CopyParams.NewerOnly ? cpNewerOnly : 0);
          Terminal->CopyToLocal(FInternalDDDownloadList, TargetDirectory,
            &CopyParams, Params);
          FInternalDDDownloadList->Clear();
        }
      }
      DoOperation = false;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::RemoteFileControlDDFileOperationExecuted(
  TObject * /*Sender*/, int dwEffect, AnsiString /*SourcePath*/,
  AnsiString /*TargetPath*/)
{
  if ((dwEffect == DROPEFFECT_Move) &&
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
  if (FCurrentSide != Side)
  {
    CommandLineCombo->Items->Clear();
    FCommandLineComboPopulated = false;
  }
  TCustomScpExplorerForm::SideEnter(Side);
  UpdateControls();
  LocalPathLabel->UpdateStatus();
  RemotePathLabel->UpdateStatus();
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::OpenConsole(AnsiString Command)
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
void __fastcall TScpCommanderForm::CommandLineComboKeyDown(TObject * /*Sender*/,
  WORD & Key, TShiftState /*Shift*/)
{
  if (Key == VK_RETURN)
  {
    Key = 0;
    ExecuteCommandLine();
  }
  else if ((Key == VK_ESCAPE) && !CommandLineCombo->DroppedDown)
  {
    Key = 0;
    CommandLineCombo->Text = "";
  }
  else if ((Key == VK_UP) || (Key == VK_DOWN))
  {
    CommandLinePopulate();
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::SaveCommandLine()
{
  if (FCommandLineComboPopulated)
  {
    CustomWinConfiguration->History[
      FCurrentSide == osRemote ? "Commands" : "LocalCommands"] =
        CommandLineCombo->Items;
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::ExecuteCommandLine()
{
  if (!CommandLineCombo->Text.Trim().IsEmpty() &&
      ((FCurrentSide != osRemote) ||
       (Terminal->AllowedAnyCommand(CommandLineCombo->Text))))
  {
    CommandLinePopulate();
    CommandLineCombo->SaveToHistory();
    AnsiString Command = CommandLineCombo->Text;
    CommandLineCombo->Text = "";
    if (FCurrentSide == osRemote)
    {
      OpenConsole(Command);
    }
    else
    {
      AnsiString Program, Params, Dir;
      SplitCommand(Command, Program, Params, Dir);
      if (!ExecuteShell(Program, Params))
      {
        throw Exception(FMTLOAD(EXECUTE_APP_ERROR, (Program)));
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::CommandLineComboDropDown(
  TObject * /*Sender*/)
{
  CommandLinePopulate();
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::CommandLinePopulate()
{
  if (!FCommandLineComboPopulated)
  {
    TStrings * CommandsHistory;
    CommandsHistory = CustomWinConfiguration->History[
      FCurrentSide == osRemote ? "Commands" : "LocalCommands"];
    if ((CommandsHistory != NULL) && (CommandsHistory->Count > 0))
    {
      CommandLineCombo->Items = CommandsHistory;
    }
    else
    {
      CommandLineCombo->Items->Clear();
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
    CommandLineCombo->SetFocus();
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
void __fastcall TScpCommanderForm::CommandLineComboEnter(TObject * /*Sender*/)
{
  KeyPreview = false;
  TPanel * LastPanel = FCurrentSide == osLocal ? LocalPanel : RemotePanel;
  MakeNextInTabOrder(LastPanel, CommandLinePanel);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::CommandLineComboExit(TObject * /*Sender*/)
{
  KeyPreview = true;
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::PanelExportStore(TOperationSide Side,
  TPanelExport Export, TPanelExportDestination Destination,
  TStringList * ExportData)
{
  if (Destination == pedCommandLine)
  {
    ComponentVisible[fcCommandLinePanel] = true;
    
    AnsiString Buf;
    for (int Index = 0; Index < ExportData->Count; Index++)
    {
      Buf += ExportData->Strings[Index] + " ";
    }
    
    if (CommandLineCombo->Focused())
    {
      CommandLineCombo->SelText = Buf;
    }
    else
    {
      CommandLineCombo->Text = CommandLineCombo->Text + Buf;
    }
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
    (CommandLinePanel->Visible ? CommandLinePanel->Height : 0) +
    (ToolbarPanel->Visible ? ToolbarPanel->Height : 0) +
    (StatusBar->Visible ? StatusBar->Height : 0);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::Resize()
{
  TCustomScpExplorerForm::Resize();

  LocalPanelWidth = FLastLocalPanelWidth;
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::StatusBarDblClick(TObject * /*Sender*/)
{
  DoFileSystemInfoDialog(Terminal);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::LocalFileControlDDMenuPopup(TObject * /*Sender*/,
  HMENU AMenu, IDataObject * /*DataObj*/, int /*AMinCustCmd*/, int /*grfKeyState*/,
  TPoint & /*pt*/)
{
  if (IsFileControl(DropSourceControl, osRemote) &&
      !WinConfiguration->DDAllowMoveInit)
  {
    // index of copy item
    int Index = GetMenuDefaultItem(AMenu, TRUE, 0);
    assert(Index >= 0);

    AnsiString Caption = Dragdrop_MIMoveStr;

    MENUITEMINFO MI;
    memset(&MI, 0, sizeof(MI));
    MI.cbSize = sizeof(MI);
    MI.fMask = MIIM_TYPE | MIIM_ID | MIIM_STATE;
    MI.fType = MFT_STRING;
    MI.wID = 1 /*DragDrop::CmdMove*/;
    MI.dwTypeData = Caption.c_str();
    MI.cch = Caption.Length();
    MI.fState = MFS_ENABLED;
    InsertMenuItem(AMenu, Index, TRUE, &MI);

    if (FDDMoveSlipped)
    {
      SetMenuDefaultItem(AMenu, Index, TRUE);  
    }

    FDDMoveSlipped = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::PathLabelDblClick(TObject * Sender)
{
  OpenDirectory(Sender == LocalPathLabel ? osLocal : osRemote);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::LocalPathLabelGetStatus(
  TCustomPathLabel * /*Sender*/, bool & Active)
{
  if (LocalDriveView->Focused())
  {
    Active = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::RemotePathLabelGetStatus(
  TCustomPathLabel * /*Sender*/, bool & Active)
{
  if (RemoteDriveView->Focused())
  {
    Active = true;
  }
}
//---------------------------------------------------------------------------

