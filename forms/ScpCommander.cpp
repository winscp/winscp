//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "ScpCommander.h"

#include <Common.h>
#include <Net.h>
#include <ScpMain.h>
#include <Interface.h>
#include <TextsWin.h>
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
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
__fastcall TScpCommanderForm::TScpCommanderForm(TComponent* Owner)
        : TCustomScpExplorerForm(Owner)
{
  FLastDirView = LocalDirView;
  FLastLocalPanelWidth = LocalPanelWidth;
  FSynchronization = ssStopped;
  FSynchronizeDialog = NULL;
  FSynchronisingBrowse = false;
  FFirstTerminal = true;

  LocalBackButton->DropdownMenu = LocalDirView->BackMenu;
  LocalForwardButton->DropdownMenu = LocalDirView->ForwardMenu;
  RemoteBackButton->DropdownMenu = RemoteDirView->BackMenu;
  RemoteForwardButton->DropdownMenu = RemoteDirView->ForwardMenu;
  SavedSessionsButton->OnClick = DropDownButtonMenu;

  TopCoolBar->PopupMenu = NonVisualDataModule->CommanderBarPopup;
  ToolbarPanel->PopupMenu = TopCoolBar->PopupMenu;
  StatusBar->PopupMenu = TopCoolBar->PopupMenu;

  LocalCoolBar->PopupMenu = NonVisualDataModule->LocalPanelPopup;
  LocalPathLabel->PopupMenu = LocalCoolBar->PopupMenu;
  LocalStatusBar->PopupMenu = LocalCoolBar->PopupMenu;

  RemoteCoolBar->PopupMenu = NonVisualDataModule->RemotePanelPopup;
  RemotePathLabel->PopupMenu = RemoteCoolBar->PopupMenu;
  RemoteStatusBar->PopupMenu = RemoteCoolBar->PopupMenu;

  // set common norton shorcuts to our actions
  NonVisualDataModule->CommanderShortcuts();
  Splitter->ShowHint = True;
  ((TLabel*)Splitter)->OnDblClick = SplitterDblClick;
  RemotePathComboBox->TabStop = False;

  LocalDirView->Font = Screen->IconFont;
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
  FDirViewToSelect = (WinConfiguration->ScpCommander.CurrentPanel == osLocal ?
    (TCustomDirView *)LocalDirView : (TCustomDirView *)RemoteDirView);
  #define RESTORE_PANEL_PARAMS(PANEL) \
    PANEL ## DirView->ColProperties->ParamsStr = WinConfiguration->ScpCommander.PANEL ## Panel.DirViewParams; \
    PANEL ## StatusBar->Visible = WinConfiguration->ScpCommander.PANEL ## Panel.StatusBar; \
    LoadCoolbarLayoutStr(PANEL ## CoolBar, WinConfiguration->ScpCommander.PANEL ## Panel.CoolBarLayout)
  RESTORE_PANEL_PARAMS(Local);
  RESTORE_PANEL_PARAMS(Remote);
  #undef RESTORE_PANEL_PARAMS
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
    WinConfiguration->ScpCommander.CurrentPanel =
      ((FLastDirView == LocalDirView) ? osLocal : osRemote);

    #define STORE_PANEL_PARAMS(PANEL) \
      WinConfiguration->ScpCommander.PANEL ## Panel.DirViewParams = PANEL ## DirView->ColProperties->ParamsStr; \
      WinConfiguration->ScpCommander.PANEL ## Panel.StatusBar = PANEL ## StatusBar->Visible; \
      WinConfiguration->ScpCommander.PANEL ## Panel.CoolBarLayout = GetCoolbarLayoutStr(PANEL ## CoolBar)
    STORE_PANEL_PARAMS(Local);
    STORE_PANEL_PARAMS(Remote);
    #undef RESTORE_PANEL_PARAMS

    WinConfiguration->ScpCommander.WindowParams = WinConfiguration->StoreForm(this);;
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
  if (Data->UpdateDirectories || (Data != Terminal->SessionData))
  {
    assert(LocalDirView);
    Data->LocalDirectory = LocalDirView->PathName;
    Terminal->UserObject = NULL;
  }
  else
  {
    if (!Terminal->UserObject)
    {
      Terminal->UserObject = new TTerminalUserObject();
    }
    dynamic_cast<TTerminalUserObject *>(Terminal->UserObject)->LocalDirectory =
      LocalDirView->PathName;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TScpCommanderForm::CopyParamDialog(TTransferDirection Direction,
  TTransferType Type, bool DragDrop, TStrings * FileList, AnsiString & TargetDirectory,
  TCopyParamType & CopyParam, bool Confirm)
{
  if (DragDrop && (Direction == tdToLocal) && (FDDTargetDirView == LocalDirView))
  {
    if (LocalDirView->DropTarget)
    {
      // when drop target is not directory, it is probably file type, which have
      // associated drop handler (sich as ZIP file in WinXP). in this case we
      // must leave drop handling to destination application.
      DragDrop = !LocalDirView->ItemIsDirectory(LocalDirView->DropTarget);
      if (!DragDrop)
      {
        TargetDirectory = LocalDirView->ItemFullFileName(LocalDirView->DropTarget);
      }
    }
    else
    {
      DragDrop = false;
      TargetDirectory = IncludeTrailingBackslash(LocalDirView->Path);
    }
  }
  else if (!DragDrop)
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

  return TCustomScpExplorerForm::CopyParamDialog(Direction, Type, DragDrop,
    FileList, TargetDirectory, CopyParam, Confirm);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::FormShow(TObject */*Sender*/)
{
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
     ((FLAG & afLocal) && (FLastDirView == LocalDirView)) ||
     ((FLAG & afRemote) && (FLastDirView == RemoteDirView))
    );
  #undef FLAG
}
//---------------------------------------------------------------------------
TCustomDirView * __fastcall TScpCommanderForm::DirView(TOperationSide Side)
{
  switch (Side) {
    case osCurrent: return FLastDirView;
    case osLocal: return LocalDirView;
    case osRemote: return RemoteDirView;
    default: assert(false); return NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::ExecuteFileOperation(::TFileOperation Operation, TOperationSide Side, Boolean OnFocused)
{
  TCustomScpExplorerForm::ExecuteFileOperation(Operation, Side, OnFocused);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::TerminalChanged()
{
  TCustomScpExplorerForm::TerminalChanged();
  if (Terminal)
  {
    if (FFirstTerminal || !WinConfiguration->ScpCommander.PreserveLocalDirectory)
    {
      AnsiString LocalDirectory;

      if (Terminal->UserObject)
      {
        LocalDirectory = dynamic_cast<TTerminalUserObject *>(Terminal->UserObject)->LocalDirectory;
      }
      else
      {
        LocalDirectory = Terminal->SessionData->LocalDirectory;
      }
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
          ShowExtendedException(&E, this);
        }
      }

      if (DocumentsDir)
      {
        LocalDirView->HomeDirectory = "";
        LocalDirView->ExecuteHomeDirectory();
      }

      if (Configuration->DefaultDirIsHome &&
          !Terminal->SessionData->UpdateDirectories)
      {
        LocalDirView->HomeDirectory = LocalDirectory;
      }
    }
    FFirstTerminal = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::ConfigurationChanged()
{
  TCustomScpExplorerForm::ConfigurationChanged();
  if (Configuration->DefaultDirIsHome && Terminal)
  {
    LocalDirView->HomeDirectory = Terminal->SessionData->LocalDirectory;
  }
  else
  {
    LocalDirView->HomeDirectory = "";
  }
  LocalDirView->DimmHiddenFiles = WinConfiguration->DimmHiddenFiles;
  LocalDirView->ShowHiddenFiles = WinConfiguration->ShowHiddenFiles;

  LocalDirView->NortonLike = !WinConfiguration->ScpCommander.ExplorerStyleSelection;
  RemoteDirView->NortonLike = !WinConfiguration->ScpCommander.ExplorerStyleSelection;
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
void __fastcall TScpCommanderForm::UpdateControls()
{
  Splitter->Hint = FormatFloat("0%|X", LocalPanelWidth*100);
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
  assert(FLastDirView);
  FLastDirView->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::SessionComboCloseUp(TObject *Sender)
{
  PathComboBoxCloseUp(Sender, false);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::FormResize(TObject * /*Sender*/)
{
  LocalPanelWidth = FLastLocalPanelWidth;
  UpdateControls();
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
    default: return TCustomScpExplorerForm::GetComponent(Component);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::SetComponentVisible(Word Component, Boolean value)
{
  TCustomScpExplorerForm::SetComponentVisible(Component, value);
  if (StatusBar->Top < ToolbarPanel->Top)
  {
    StatusBar->Top = ToolbarPanel->Top + ToolbarPanel->Height;
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::KeyDown(Word & Key, Classes::TShiftState Shift)
{
  // duplicate shortcut for deleting
  if ((ShortCut(VK_DELETE, TShiftState()) == ShortCut(Key, Shift)) &&
      !DirView(osCurrent)->IsEditing())
  {
    NonVisualDataModule->CurrentDeleteAction->Execute();
    Key = 0;
  }
  else
  {
    TCustomScpExplorerForm::KeyDown(Key, Shift);
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
  TSynchronizeParamType Params;
  Params.CopyParams.Assign(Configuration->CopyParam);
  Params.AllowTransferMode = Terminal->IsCapable[fcTextMode];
  if (!Params.AllowTransferMode)
  {
    Params.CopyParams.TransferMode = tmBinary;
  }
  Params.LocalDirectory = LocalDirView->PathName;
  Params.RemoteDirectory = RemoteDirView->PathName;
  DoSynchronizeDialog(Params, SynchronizeStartStop);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::SynchronizeStartStop(System::TObject* Sender,
  bool Start, TSynchronizeParamType Params)
{
  FSynchronization = (Start ? ssWaiting : ssStopped);
  if (Start)
  {
    FSynchronizeDialog = (TSynchronizeDialog *)Sender;
    FSynchronizeParams = Params;
    SynchronizeNow();
  }
  else
  {
    FSynchronizeDialog = NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::LocalDirViewChangeDetected(
      TObject * /*Sender*/)
{
  switch (FSynchronization) {
    case ssWaiting: SynchronizeNow(); break;
    case ssSynchronizing: FSynchronization = ssSynchronize; break;
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::SynchronizeNow()
{
  try
  {
    TStrings * ChangedFiles;
    FSynchronization = ssSynchronize;
    try
    {
      // repeat until there is any change pending (see ::LocalDirViewChangeDetected)
      while (FSynchronization == ssSynchronize)
      {
        FSynchronization = ssSynchronizing;
        assert(FSynchronizeDialog);
        ChangedFiles = LocalDirView->CreateChangedFileList(
          RemoteDirView, true, FSynchronizeDialog->ExistingOnly,
          WinConfiguration->ScpCommander.CompareCriterias());
          
        Terminal->ExceptionOnFail = true;
        try
        {
          if (ChangedFiles->Count > 0)
          {
            Terminal->CopyToRemote(ChangedFiles, FSynchronizeParams.RemoteDirectory,
              &FSynchronizeParams.CopyParams, 0);
          }
        }
        __finally
        {
          delete ChangedFiles;
          Terminal->ExceptionOnFail = false;
        }
      }
    }
    __finally
    {
      FSynchronization = ssWaiting;
    }
  }
  catch (EFatal & E)
  {
    throw;
  }
  catch(Exception & E)
  {
    assert(FSynchronizeDialog);
    FSynchronizeDialog->Stop();
    ShowExtendedException(&E);
  }
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::DoOperationFinished(
  ::TFileOperation Operation, TOperationSide Side,
  bool DragDrop, const AnsiString FileName, bool Success,
  bool & DisconnectWhenFinished)
{
  if (FSynchronization == ssStopped)
  {
    TCustomScpExplorerForm::DoOperationFinished(Operation, Side, DragDrop,
      FileName, Success, DisconnectWhenFinished);
  }
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
void __fastcall TScpCommanderForm::LocalDirViewDDDragEnter(TObject *Sender,
      IDataObject *DataObj, int grfKeyState, TPoint &Point, int &dwEffect,
      bool &Accept)
{
  // LocalDirViewDDDragEnter is duplication of
  // TCustomScpExplorerForm::DirViewDDDragEnter, but it differs in
  // literal type of 'DataObj' parameter.Actual type is however same
  DirViewDDDragEnter(Sender, DataObj, grfKeyState, Point, dwEffect, Accept);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::FileOperationProgress(
  TFileOperationProgressType & ProgressData, TCancelStatus & Cancel)
{
  // Heuristic: When operation finishes and DD targed is local dir view,
  // we suppose that drag&drop download finished, so local dir view should be
  // reloaded
  if (!ProgressData.InProgress && FProgressForm &&
      (FDDTargetDirView == LocalDirView))
  {
    LocalDirView->ReloadDirectory();
  }
  TCustomScpExplorerForm::FileOperationProgress(ProgressData, Cancel);
}
//---------------------------------------------------------------------------
void __fastcall TScpCommanderForm::DirViewLoaded(TObject *Sender)
{
  try
  {
    TCustomDirView * ADirView = dynamic_cast<TCustomDirView *>(Sender);
    assert(ADirView);
    AnsiString PrevPath = FPrevPath[ADirView == LocalDirView];
    FPrevPath[ADirView == LocalDirView] = ADirView->Path;
    
    if (!FSynchronisingBrowse && NonVisualDataModule->SynchorizeBrowsingAction->Checked &&
        !PrevPath.IsEmpty() && PrevPath != ADirView->Path)
    {
      FSynchronisingBrowse = true;
      if (ADirView == LocalDirView)
      {
        Terminal->ExceptionOnFail = true;
        try
        {
          if (PrevPath == ExcludeTrailingBackslash(ExtractFilePath(LocalDirView->Path)))
          {
            RemoteDirView->Path = RemoteDirView->Path + ExtractFileName(LocalDirView->Path);
          }
          else if (ExcludeTrailingBackslash(ExtractFilePath(PrevPath)) ==
                    ExcludeTrailingBackslash(LocalDirView->PathName))
          {
            if (RemoteDirView->IsRoot)
            {
              Abort();
            }
            RemoteDirView->Path = UnixExtractFilePath(RemoteDirView->PathName);
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
        if (PrevPath == UnixExtractFilePath(RemoteDirView->PathName))
        {
          LocalDirView->Path = IncludeTrailingBackslash(LocalDirView->Path) +
            UnixExtractFileName(RemoteDirView->PathName);
        }
        else if (UnixExtractFilePath(UnixExcludeTrailingBackslash(PrevPath)) == RemoteDirView->Path)
        {
          if (LocalDirView->IsRoot)
          {
            Abort();
          }
          LocalDirView->Path = ExtractFilePath(LocalDirView->Path);
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
    NonVisualDataModule->SynchorizeBrowsingAction->Checked = false;
    if (!Application->Terminated)
    {
      ShowExtendedException(&E);
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
  if (FLastDirView == LocalDirView)
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


