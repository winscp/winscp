//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Clipbrd.hpp>

#include "CustomScpExplorer.h"

#include <Common.h>
#include <Interface.h>
#include <Net.h>
#include <ScpMain.h>
#include <FileSystems.h>
#include <TextsWin.h>
#include <DiscMon.hpp>

#include <VCLCommon.h>
#include <Log.h>
#include <Progress.h>
#include <SynchronizeProgress.h>
#include <OperationStatus.h>

#include <DragExt.h>

#include "GUITools.h"
#include "NonVisual.h"
#include "Tools.h"
#include "WinConfiguration.h"
#include "TerminalManager.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "CustomDirView"
#pragma link "CustomUnixDirView"
#pragma link "IEListView"
#pragma link "NortonLikeListView"
#pragma link "UnixDirView"
#pragma link "AssociatedStatusBar"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
#ifdef DEBUGMODE
#define DEBUG(MSG) CurrentSSH->LogEvent(MSG)
#else
#define DEBUG(MSG)
#endif
//---------------------------------------------------------------------------
#define SAVE_SELECTION(DIRVIEW) \
  AnsiString FocusFile = ""; \
  AnsiString LastFocusedFile = ""; \
  if (DIRVIEW->ItemFocused) LastFocusedFile = DIRVIEW->ItemFocused->Caption; \
  { TListItem * ClosestUnselected = DIRVIEW->ClosestUnselected(DIRVIEW->ItemFocused); \
  if (ClosestUnselected) FocusFile = ClosestUnselected->Caption; }
#define RESTORE_SELECTION(DIRVIEW) \
  if (!LastFocusedFile.IsEmpty() && \
      (!DIRVIEW->ItemFocused || (DIRVIEW->ItemFocused->Caption != LastFocusedFile))) \
  { \
    TListItem *ItemToSelect = DIRVIEW->FindFileItem(FocusFile); \
    if (ItemToSelect) \
    { \
      DIRVIEW->ItemFocused = ItemToSelect; \
      DIRVIEW->ItemFocused->MakeVisible(False); \
    } \
  }
//---------------------------------------------------------------------------
class TMutexGuard
{
public:
  TMutexGuard(HANDLE AMutex, int Message = MUTEX_RELEASE_TIMEOUT,
    int Timeout = 5000)
  {
    FMutex = NULL;
    unsigned long WaitResult = WaitForSingleObject(AMutex, Timeout);
    if (WaitResult == WAIT_TIMEOUT)
    {
      throw Exception(LoadStr(MUTEX_RELEASE_TIMEOUT));
    }
    else
    {
      FMutex = AMutex;
    }
  }

  ~TMutexGuard()
  {
    if (FMutex != NULL)
    {
      ReleaseMutex(FMutex);
    }
  }

private:
  HANDLE FMutex;
};
//---------------------------------------------------------------------------
__fastcall TCustomScpExplorerForm::TCustomScpExplorerForm(TComponent* Owner):
    FLastDirView(NULL), FFormRestored(False), TForm(Owner)
{
  RestoreParams();
  RemoteDirView->Invalidate();
  assert(NonVisualDataModule && !NonVisualDataModule->ScpExplorer);
  NonVisualDataModule->ScpExplorer = this;
  Application->OnHint = ApplicationHint;
  FAutoOperation = false;
  FForceExecution = false;
  FShowStatusBarHint = false;
  FIgnoreNextSysCommand = false;
  FErrorList = NULL;
  FSynchronizeProgressForm = NULL;
  FProgressForm = NULL;
  FDDExtDropEffect = DROPEFFECT_NONE;
  FDDExtMapFile = NULL;
  FDDExtMutex = CreateMutex(NULL, false, DRAG_EXT_MUTEX);
  assert(FDDExtMutex != NULL);

  UseSystemSettings(this);

  TComboBox * SessionCombo = dynamic_cast<TComboBox*>(GetComponent(fcSessionCombo));
  if (SessionCombo != NULL)
  {
    SessionCombo->OnDrawItem = SessionComboDrawItem;
    SessionCombo->OnDropDown = SessionComboDropDown;
    SessionCombo->OnChange = SessionComboChange;
    SessionCombo->Hint = NonVisualDataModule->OpenedSessionsAction->Hint;
  }

  TToolBar * MenuToolBar = dynamic_cast<TToolBar*>(GetComponent(fcMenuToolBar));
  assert(MenuToolBar);
  MenuToolBar->Font = Screen->MenuFont;
  assert(MenuToolBar->ControlCount);
  MenuToolBar->Height = MenuToolBar->Controls[0]->Height;

  RemoteDirView->Font = Screen->IconFont;
  RemoteDirView->DDAllowMove = true;
}
//---------------------------------------------------------------------------
__fastcall TCustomScpExplorerForm::~TCustomScpExplorerForm()
{
  CloseHandle(FDDExtMutex);
  FDDExtMutex = NULL;

  assert(!FErrorList);
  StoreParams();
  Terminal = NULL;
  assert(NonVisualDataModule && (NonVisualDataModule->ScpExplorer == this));
  NonVisualDataModule->ScpExplorer = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SetTerminal(TTerminal * value)
{
  if (FTerminal != value)
  {
    if (FTerminal)
    {
      UpdateSessionData(Terminal->SessionData);
    }
    FTerminal = value;
    TerminalChanged();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::TerminalChanged()
{
  RemoteDirView->Terminal = Terminal;
  Caption = Application->Title;
  if (Terminal)
  {
    UpdateStatusBar();
  }
  TerminalListChanged(NULL);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ConfigurationChanged()
{
  assert(Configuration && RemoteDirView);
  RemoteDirView->DimmHiddenFiles = WinConfiguration->DimmHiddenFiles;
  RemoteDirView->ShowHiddenFiles = WinConfiguration->ShowHiddenFiles;
  RemoteDirView->ShowInaccesibleDirectories = WinConfiguration->ShowInaccesibleDirectories;
  RemoteDirView->DDTemporaryDirectory = WinConfiguration->DDTemporaryDirectory;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteDirViewGetCopyParam(
      TUnixDirView * /*Sender*/, TTransferDirection Direction,
      TTransferType Type, AnsiString &TargetDirectory, TStrings * FileList,
      TCopyParamType &CopyParam)
{
  if (!CopyParamDialog(Direction, Type, true, FileList,
        TargetDirectory, CopyParam, WinConfiguration->DDTransferConfirmation))
  {
    Abort();
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::CopyParamDialog(
  TTransferDirection Direction, TTransferType Type, bool DragDrop,
  TStrings * FileList, AnsiString & TargetDirectory, TCopyParamType & CopyParam,
  bool Confirm)
{
  bool Result = true;
  assert(Terminal && Terminal->Active);
  if (DragDrop && (Direction == tdToLocal) && (Type == ttMove) &&
      !WinConfiguration->DDAllowMove)
  {
    int Answer = MessageDialog(LoadStr(DND_DOWNLOAD_MOVE_WARNING), qtWarning,
      qaOK | qaCancel, 0, mpNeverAskAgainCheck);
    if (Answer == qaNeverAskAgain)
    {
      WinConfiguration->DDAllowMove = true;
    }
    else if (Answer == qaCancel)
    {
      Result = false;
    }
  }

  if (Result && Confirm)
  {
    Result = DoCopyDialog(Direction == tdToRemote, Type == ttMove,
      DragDrop, FileList, Terminal->IsCapable[fcTextMode], TargetDirectory,
      &CopyParam, true);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RestoreFormParams()
{
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RestoreParams()
{
  // IDE often looses this link
  RemoteDirView->HeaderImages = NonVisualDataModule->ArrowImages;

  if (Position == poDesigned)
  {
    RestoreFormParams();
  }
  ConfigurationChanged();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::StoreParams()
{
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CreateParams(TCreateParams & Params)
{
  if (!FFormRestored)
  {
    FFormRestored = true;
    RestoreFormParams();
  }
  TForm::CreateParams(Params);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FileOperationProgress(
  TFileOperationProgressType & ProgressData, TCancelStatus & /*Cancel*/)
{
  // operation is being executed and we still didn't show up progress form
  if (ProgressData.InProgress && !FProgressForm)
  {
    //assert(Screen && Screen->ActiveCustomForm);
    FProgressForm = new TProgressForm(Application);
    FProgressForm->DeleteToRecycleBin = WinConfiguration->DeleteToRecycleBin;
    // When main window is hidden, we suppose "/upload" mode
    if (!Visible)
    {
      FProgressForm->DisconnectWhenComplete = true;
    }
  }
  // operation is finished (or terminated), so we hide progress form
  else if (!ProgressData.InProgress && FProgressForm)
  {
    SAFE_DESTROY(FProgressForm);
  }

  if (FProgressForm)
  {
    FProgressForm->SetProgressData(ProgressData);
    if (FProgressForm->Cancel > ProgressData.Cancel)
    {
      ProgressData.Cancel = FProgressForm->Cancel;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::OperationProgress(
  TFileOperationProgressType & ProgressData, TCancelStatus & Cancel)
{
  FileOperationProgress(ProgressData, Cancel);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DoOperationFinished(
  TFileOperation Operation, TOperationSide Side,
  bool DragDrop, const AnsiString FileName, bool Success,
  bool & DisconnectWhenComplete)
{
  if (!FAutoOperation)
  {
    // no selection on "/upload", form servers only as event handler
    // (it is not displayed)
    if (!DragDrop && Visible && (Operation != foCalculateSize))
    {
      TListItem *Item = DirView(Side)->FindFileItem(FileName);
      assert(Item);
      if (Success) Item->Selected = false;
      Item->MakeVisible(false);
    }
    if (FProgressForm)
    {
      DisconnectWhenComplete = FProgressForm->DisconnectWhenComplete;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::OperationFinished(
  TFileOperation Operation, TOperationSide Side,
  bool DragDrop, const AnsiString FileName, Boolean Success,
  bool & DisconnectWhenComplete)
{
  DoOperationFinished(Operation, Side, DragDrop, FileName, Success,
    DisconnectWhenComplete);
}
//---------------------------------------------------------------------------
TCustomDirView * __fastcall TCustomScpExplorerForm::DirView(TOperationSide Side)
{
  assert((Side == osRemote) || ((Side == osCurrent) && FLastDirView));
  return (Side == osCurrent) ? FLastDirView : RemoteDirView;
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::GetEnableFocusedOperation(TOperationSide Side)
{
  return DirView(Side)->AnyFileSelected(true);
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::GetEnableSelectedOperation(TOperationSide Side)
{
  return DirView(Side)->AnyFileSelected(false);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteDirViewContextPopup(
      TObject * /*Sender*/, const TPoint &MousePos, bool &Handled)
{
  TListItem * Item = RemoteDirView->ItemFocused;
  if ((RemoteDirView->GetItemAt(MousePos.x, MousePos.y) == Item) &&
      RemoteDirView->AnyFileSelected(true))
  {
    TPoint ScreenPoint, ClientPoint;
    ClientPoint = ((MousePos.x < 0) && (MousePos.y < 0)) ?
      TPoint(0, 0) : MousePos;
    ScreenPoint = RemoteDirView->ClientToScreen(ClientPoint);

    NonVisualDataModule->CurrentOpenMenuItem->Default = !WinConfiguration->CopyOnDoubleClick;
    NonVisualDataModule->CurrentCopyMenuItem->Default = WinConfiguration->CopyOnDoubleClick;
    NonVisualDataModule->CurrentOpenMenuItem->Visible = WinConfiguration->ExpertMode;
    NonVisualDataModule->CurentEditMenuItem->Visible = WinConfiguration->ExpertMode;

    reinterpret_cast<TPopupMenu*>(GetComponent(fcRemotePopup))->Popup(
      ScreenPoint.x, ScreenPoint.y);
  }
  Handled = true;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::BatchStart(void *& Storage)
{
  assert(FErrorList == NULL);

  Storage = new bool;
  if (HasDirView[osLocal])
  {
    *static_cast<bool*>(Storage) = DirView(osLocal)->WatchForChanges;
    DirView(osLocal)->WatchForChanges = false;
  }

  if (WinConfiguration->ContinueOnError)
  {
    FErrorList = new TStringList();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::BatchEnd(void * Storage)
{
  assert(Storage != NULL);

  if (FErrorList)
  {
    HandleErrorList(FErrorList);
  }

  if (HasDirView[osLocal])
  {
    DirView(osLocal)->WatchForChanges = *static_cast<bool*>(Storage);
  }

  delete Storage;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ExecuteFileOperation(TFileOperation Operation,
  TOperationSide Side, TStrings * FileList, bool NoConfirmation, void * Param)
{
  void * BatchStorage;
  BatchStart(BatchStorage);
  try
  {
    if ((Operation == foCopy) || (Operation == foMove))
    {
      TTransferDirection Direction = (Side == osLocal ? tdToRemote : tdToLocal);
      TTransferType Type = (Operation == foCopy ? ttCopy : ttMove);
      AnsiString TargetDirectory;
      if (Param != NULL)
      {
        TargetDirectory = *static_cast<AnsiString*>(Param);
      }
      TCopyParamType CopyParam = Configuration->CopyParam;
      if (CopyParamDialog(Direction, Type, false, FileList, TargetDirectory,
          CopyParam, !NoConfirmation))
      {
        assert(Terminal);
        TCustomDirView * DView = DirView(Side);
        bool SelectionRestored = false;
        DView->SaveSelection();

        try
        {
          if (Side == osLocal)
          {
            int Params = 0;
            if (Operation == foMove) Params |= cpDelete;
            Terminal->CopyToRemote(FileList, TargetDirectory, &CopyParam, Params);
            if (Operation == foMove)
            {
              DView->Reload(True);
              DView->RestoreSelection();
              SelectionRestored = true;
            }
          }
          else
          {
            try
            {
              Terminal->CopyToLocal(FileList, TargetDirectory, &CopyParam,
                (Operation == foMove ? cpDelete : 0));
            }
            __finally
            {
              if (Operation == foMove)
              {
                DView->RestoreSelection();
                SelectionRestored = true;
              }
              if (HasDirView[osLocal] &&
                  (IncludeTrailingBackslash(TargetDirectory) ==
                    IncludeTrailingBackslash(DirView(osLocal)->Path)))
              {
                DirView(osLocal)->ReloadDirectory();
              }
            }
          }
        }
        __finally
        {
          if (!SelectionRestored)
          {
            DView->DiscardSavedSelection();
          }
        }
      }
    }
    else if (Operation == foRename)
    {
      assert(DirView(Side)->ItemFocused);
      DirView(Side)->ItemFocused->EditCaption();
    }
    else if (Operation == foDelete)
    {
      assert(FileList->Count);
      bool Confirmed = !WinConfiguration->ConfirmDeleting;
      if (!Confirmed)
      {
        AnsiString Query;
        if (FileList->Count == 1)
        {
          if (Side == osLocal)
          {
            Query = ExtractFileName(FileList->Strings[0]);
          }
          else
          {
            Query = UnixExtractFileName(FileList->Strings[0]);
          }
          Query = FMTLOAD(CONFIRM_DELETE_FILE, (Query));
        }
        else
        {
          Query = FMTLOAD(CONFIRM_DELETE_FILES, (FileList->Count));
        }

        int Answer = MessageDialog(Query, qtConfirmation,
          qaOK | qaCancel, 0, mpNeverAskAgainCheck);
        if (Answer == qaNeverAskAgain)
        {
          Confirmed = true;
          WinConfiguration->ConfirmDeleting = false;
        }
        else
        {
          Confirmed = (Answer == qaOK);
        }
      }

      if (Confirmed) DeleteFiles(Side, FileList);
    }
    else if (Operation == foSetProperties)
    {
      SetProperties(Side, FileList);
    }
    else if (Operation == foCustomCommand)
    {
      assert(Param);
      assert(Side == osRemote);

      FCustomCommandName = *((AnsiString *)Param);
      try
      {
        AnsiString Command = TCustomFileSystem::CompleteCustomCommand(
          WinConfiguration->CustomCommands->Values[FCustomCommandName],
          "", CustomCommandGetParamValue);
        Terminal->CustomCommandOnFiles(Command,
          WinConfiguration->CustomCommands->Params[FCustomCommandName],
          FileList);
      }
      __finally
      {
        FCustomCommandName = "";
      }
    }
    else if (Operation == foRemoteMove)
    {
      assert(Side == osRemote);
      RemoteMoveFiles(FileList, NoConfirmation);
    }
    else
    {
      assert(false);
    }
  }
  __finally
  {
    BatchEnd(BatchStorage);
  }
}
//---------------------------------------------------------------------------
TOperationSide __fastcall TCustomScpExplorerForm::GetSide(TOperationSide Side)
{
  if (Side == osCurrent)
  {
    Side = osRemote;
  }

  return Side;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ExecuteFileOperation(TFileOperation Operation,
  TOperationSide Side, bool OnFocused, bool NoConfirmation, void * Param)
{
  Side = GetSide(Side);

  TStrings * FileList = DirView(Side)->CreateFileList(OnFocused, (Side == osLocal), NULL);
  try
  {
    ExecuteFileOperation(Operation, Side, FileList, NoConfirmation, Param);
  }
  __finally
  {
    delete FileList;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CustomCommandGetParamValue(
  const AnsiString AName, AnsiString & Value)
{
  assert(!FCustomCommandName.IsEmpty());
  AnsiString Name = AName;
  if (Name.IsEmpty())
  {
    Name = FMTLOAD(CUSTOM_COMMANDS_PARAM_PROMPT, (StripHotkey(FCustomCommandName)));
  }
  if (!InputDialog(FMTLOAD(CUSTOM_COMMANDS_PARAM_TITLE,
        (StripHotkey(FCustomCommandName))), Name, Value))
  {
    Abort();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::HandleErrorList(TStringList *& ErrorList)
{
  try
  {
    if (ErrorList->Count)
    {
      if (MessageDialog(FMTLOAD(ERROR_LIST_COUNT, (ErrorList->Count)), qtError,
          qaOK | qaCancel, 0) == qaOK)
      {
        int Answer;
        int Index = 0;
        do
        {
          assert(Index >= 0 && Index < ErrorList->Count);
          Answer = MoreMessageDialog(
            FMTLOAD(ERROR_LIST_NUMBER, (Index+1, ErrorList->Count, ErrorList->Strings[Index])),
            dynamic_cast<TStrings *>(ErrorList->Objects[Index]), qtError,
            (Index ? qaPrev : 0) | (Index < ErrorList->Count - 1 ? qaNext : 0) |
            qaOK, 0);

          if (Answer == qaNext)
          {
            Index++;
          }
          if (Answer == qaPrev)
          {
            Index--;
          }
        }
        while (Answer != qaOK);
      }
    }
  }
  __finally
  {
    TStrings * List = ErrorList;
    ErrorList = NULL;
    for (int i = 0; i < List->Count; i++)
    {
      delete List->Objects[i];
    }
    delete List;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ExecuteFile(TOperationSide Side,
  TExecuteFileBy ExecuteFileBy)
{
  assert(!WinConfiguration->DisableOpenEdit);

  if (Side == osCurrent)
  {
    Side = FLastDirView == RemoteDirView ? osRemote : osLocal;
  }

  bool Edit = (ExecuteFileBy == efEditor || ExecuteFileBy == efAlternativeEditor);

  TStrings * FileList = DirView(Side)->CreateFocusedFileList(Side == osLocal);
  try
  {
    assert(FileList->Count == 1);
    if (Side == osRemote)
    {
      TCopyParamType CopyParam = Configuration->CopyParam;
      if (Edit)
      {
        CopyParam.TransferMode = tmAscii;
      }
      CopyParam.FileNameCase = ncNoChange;
      CopyParam.PreserveReadOnly = false;
      CopyParam.ResumeSupport = rsOff;

      AnsiString TempDir = UniqTempDir(WinConfiguration->DDTemporaryDirectory);
      ForceDirectories(TempDir);

      FAutoOperation = true;
      Terminal->ExceptionOnFail = true;
      try
      {
        Terminal->CopyToLocal(FileList, TempDir, &CopyParam, cpTemporary);
      }
      __finally
      {
        FAutoOperation = false;
        Terminal->ExceptionOnFail = false;
      }

      FExecutedFile = TempDir + FileList->Strings[0];
    }
    else
    {
      FExecutedFile = FileList->Strings[0];
    }
  }
  __finally
  {
    delete FileList;
  }

  Terminal->BeginTransaction();
  try
  {
    try
    {
      FExecutedFileTimestamp = FileAge(FExecutedFile);
      FFileExecutedBy = ExecuteFileBy;

      if (Edit && ((WinConfiguration->Editor.Editor == edInternal) !=
                    (ExecuteFileBy == efAlternativeEditor)))
      {
        TNotifyEvent OnFileChanged = NULL;
        AnsiString Caption = FExecutedFile;
        if (Side == osRemote)
        {
          OnFileChanged = ExecutedFileChanged;
          Caption = RemoteDirView->Path + ExtractFileName(FExecutedFile);
        }
        DoEditorForm(FExecutedFile, this, OnFileChanged, Caption);
      }
      else
      {
        TDiscMonitor * DiscMonitor = NULL;
        try
        {
          if (Side == osRemote)
          {
            DiscMonitor = new TDiscMonitor(this);
            DiscMonitor->SubTree = false;
            DiscMonitor->Filters = TMonitorFilters() << moLastWrite;
            DiscMonitor->Directory = ExtractFilePath(FExecutedFile);
            DiscMonitor->OnChange = ExecutedFileChanged;
            DiscMonitor->Active = true;
          }

          TOperationStatusForm * StatusForm = new TOperationStatusForm(Application);
          try
          {
            StatusForm->Status = LoadStr(DOCUMENT_WAIT);
            StatusForm->ShowAsModal();

            if (Edit)
            {
              AnsiString ExternalEditor, Program, Params, Dir;
              ExternalEditor = WinConfiguration->Editor.ExternalEditor;
              TWinConfiguration::ReformatFileNameCommand(ExternalEditor);
              SplitCommand(ExternalEditor, Program, Params, Dir);
              assert(Params.Pos(ShellCommandFileNamePattern) > 0);
              Params = StringReplace(Params, ShellCommandFileNamePattern,
                AddPathQuotes(FExecutedFile), TReplaceFlags() << rfReplaceAll);
              if (ExecuteShellAndWait(Program, Params) < 0)
              {
                throw Exception(FMTLOAD(EDITOR_ERROR, (Program)));
              }
            }
            else
            {
              assert(Side == osRemote);
              if (ExecuteShellAndWait(FExecutedFile, "") < 0)
              {
                throw Exception(FMTLOAD(EXECUTE_FILE_ERROR, (FExecutedFile)));
              }
            }
          }
          __finally
          {
            delete StatusForm;
          }

          if (Side == osRemote)
          {
            if (DiscMonitor)
            {
              DiscMonitor->Active = false;
            }
            // upload file if it was saved while [editor] was closed
            ExecutedFileChanged(NULL);
          }
        }
        __finally
        {
          delete DiscMonitor;
        }
      }
    }
    __finally
    {
      AnsiString FileName = FExecutedFile;
      FExecutedFile = "";
      if (Side == osRemote)
      {
        bool Deleted;
        AnsiString DirName = ExtractFilePath(FileName);

        if (WinConfiguration->ForceDeleteTempFolder)
        {
          Deleted = RecursiveDeleteFile(ExcludeTrailingBackslash(DirName), false);
        }
        else
        {
          Deleted = DeleteFile(FileName) && RemoveDir(DirName);
        }

        if (!Deleted)
        {
          throw Exception(FMTLOAD(DELETE_TEMP_EXECUTE_FILE_ERROR, (DirName)));
        }
      }
      else
      {
        DirView(osLocal)->ReloadDirectory();
      }
    }
  }
  __finally
  {
    Terminal->EndTransaction();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ExecutedFileChanged(TObject * Sender)
{
  static Uploading = 0;
  try
  {
    assert(!FExecutedFile.IsEmpty());

    if (Uploading == 1)
    {
      Uploading = 2;
    }
    else
    {
      Uploading = 1;
      try
      {
        int FileTimestamp = FileAge(FExecutedFile);
        if (FileTimestamp != FExecutedFileTimestamp)
        {
          FExecutedFileTimestamp = FileTimestamp;

          TStrings * FileList = new TStringList();
          try
          {
            FileList->Add(FExecutedFile);

            TCopyParamType CopyParam = Configuration->CopyParam;
            if (FFileExecutedBy == efEditor || FFileExecutedBy == efAlternativeEditor)
            {
              CopyParam.TransferMode = tmAscii;
            }
            CopyParam.TransferMode = tmAscii;
            CopyParam.FileNameCase = ncNoChange;
            CopyParam.PreserveRights = false;
            CopyParam.ResumeSupport = rsOff;

            FAutoOperation = true;
            Terminal->ExceptionOnFail = true;
            try
            {
              Terminal->CopyToRemote(FileList, RemoteDirView->PathName,
                &CopyParam, cpNoConfirmation);
            }
            __finally
            {
              FAutoOperation = false;
              Terminal->ExceptionOnFail = false;
            }
          }
          __finally
          {
            delete FileList;
          }
        }
      }
      __finally
      {
        if (Uploading == 2)
        {
          Uploading = 0;
          ExecutedFileChanged(this);
        }
        else
        {
          Uploading = 0;
        }
      }
    }
  }
  catch (Exception & E)
  {
    if (dynamic_cast<TDiscMonitor *> (Sender))
    {
      HandleExtendedException(&E);
    }
    else
    {
      throw;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DirViewEnter(TObject *Sender)
{
  DoDirViewEnter(dynamic_cast<TCustomDirView*>(Sender));
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DoDirViewEnter(TCustomDirView * DirView)
{
  assert(DirView != NULL);
  FLastDirView = DirView;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DeleteFiles(TOperationSide Side,
  TStrings * FileList)
{
  assert(Terminal);
  TCustomDirView * DView = DirView(Side);
  DView->SaveSelection();

  try
  {
    if (Side == osRemote)
    {
      Terminal->DeleteFiles(FileList);
    }
    else
    {
      try
      {
        Terminal->DeleteLocalFiles(FileList);
      }
      __finally
      {
        DView->Reload(true);
      }
    }
  }
  catch(...)
  {
    DView->DiscardSavedSelection();
    throw;
  }
  DView->RestoreSelection();
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::RemoteMoveDialog(TStrings * FileList,
  AnsiString & Target, AnsiString & FileMask, bool NoConfirmation)
{
  if (RemoteDirView->DropTarget != NULL)
  {
    assert(RemoteDirView->ItemIsDirectory(RemoteDirView->DropTarget));
    Target = RemoteDirView->ItemFullFileName(RemoteDirView->DropTarget);
  }
  else
  {
    Target = RemoteDirView->Path;
  }
  Target = UnixIncludeTrailingBackslash(Target);
  FileMask = "*.*";
  bool Result = true;
  if (!NoConfirmation)
  {
    Result = DoRemoteMoveDialog(FileList, Target, FileMask);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteMoveFiles(
  TStrings * FileList, bool NoConfirmation)
{
  AnsiString Target, FileMask;
  if (RemoteMoveDialog(FileList, Target, FileMask, NoConfirmation))
  {
    RemoteDirView->SaveSelection();

    try
    {
      Terminal->MoveFiles(FileList, Target, FileMask);
    }
    catch(...)
    {
      RemoteDirView->DiscardSavedSelection();
      throw;
    }
    RemoteDirView->RestoreSelection();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CreateDirectory(TOperationSide Side)
{
  AnsiString Name = LoadStr(NEW_FOLDER);
  if (InputDialog(LoadStr(CREATE_FOLDER_CAPTION), LoadStr(CREATE_FOLDER_PROMPT), Name))
  {
    DirView(Side)->CreateDirectory(Name);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::OpenDirectory(TOperationSide Side)
{
  DoOpenDirectoryDialog(odBrowse, Side);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteDirViewGetSelectFilter(
      TCustomDirView *Sender, bool Select, TFileFilter &Filter)
{
  assert(Sender);
  if (!DoSelectMaskDialog(Sender, Select, &Filter, Configuration)) Abort();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SetProperties(TOperationSide Side, TStrings * FileList)
{
  if (Side == osRemote)
  {
    TRemoteProperties CurrentProperties;

    CurrentProperties = TRemoteProperties::CommonProperties(FileList);

    int Flags = 0;
    if (Terminal->IsCapable[fcModeChanging]) Flags |= cpMode;
    if (Terminal->IsCapable[fcOwnerChanging]) Flags |= cpOwner;
    if (Terminal->IsCapable[fcGroupChanging]) Flags |= cpGroup;

    TRemoteProperties NewProperties = CurrentProperties;
    if (DoPropertiesDialog(FileList, RemoteDirView->PathName,
        Terminal->UserGroups, &NewProperties, Flags, Terminal))
    {
      NewProperties = TRemoteProperties::ChangedProperties(CurrentProperties, NewProperties);
      Terminal->ChangeFilesProperties(FileList, &NewProperties);
    }
  }
  else
  {
    DirView(Side)->DisplayPropertiesMenu();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::KeyDown(Word & Key, Classes::TShiftState Shift)
{
  if (!DirView(osCurrent)->IsEditing())
  {
    TShortCut KeyShortCut = ShortCut(Key, Shift);
    for (int Index = 0; Index < NonVisualDataModule->ExplorerActions->ActionCount; Index++)
    {
      TAction * Action = (TAction *)NonVisualDataModule->ExplorerActions->Actions[Index];
      if (((Action->ShortCut == KeyShortCut) ||
           (Action->SecondaryShortCuts->IndexOfShortCut(KeyShortCut) >= 0)) &&
          AllowedAction(Action, aaShortCut))
      {
        Key = 0;
        Action->Execute();
        return;
      }
    }
    for (int i = 0; i < NonVisualDataModule->OpenedSessionsMenu->Count; i++)
    {
      TMenuItem * Item = NonVisualDataModule->OpenedSessionsMenu->Items[i];
      if (Item->ShortCut == KeyShortCut)
      {
        Key = 0;
        Item->Click();
        FIgnoreNextSysCommand = true;
        return;
      }
    }
    if (Key == VK_TAB && Shift.Contains(ssCtrl))
    {
      TTerminalManager::Instance()->CycleTerminals(!Shift.Contains(ssShift));
    }
  }

  TForm::KeyDown(Key, Shift);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateStatusBar()
{
  TStatusBar * SessionStatusBar = (TStatusBar *)GetComponent(fcStatusBar);
  assert(SessionStatusBar && (SessionStatusBar->Panels->Count >= 3));
  if (FShowStatusBarHint)
  {
    SessionStatusBar->SimplePanel = true;
    SessionStatusBar->SimpleText = FStatusBarHint;
  }
  else if (!Terminal || !Terminal->Active || Terminal->Status < sshReady)
  {
    // note: (Terminal->Status < sshReady) currently never happens here,
    // so STATUS_CONNECTING is never used
    SessionStatusBar->SimplePanel = true;
    SessionStatusBar->SimpleText = LoadStr(
      !Terminal || !Terminal->Active ? STATUS_DISCONNECTED : STATUS_CONNECTING);
  }
  else
  {
    assert(Terminal);
    SessionStatusBar->SimplePanel = false;
    int Index = SessionStatusBar->Tag;
    SessionStatusBar->Panels->Items[Index]->Text = FormatBytes(Terminal->BytesReceived);
    SessionStatusBar->Panels->Items[Index + 1]->Text = FormatBytes(Terminal->BytesSent);
    SessionStatusBar->Panels->Items[Index + 5]->Text = Terminal->ProtocolName;
    SessionStatusBar->Panels->Items[Index + 6]->Text =
      FormatDateTime(Configuration->TimeFormat, Terminal->Duration);
  }
  SessionStatusBar->Invalidate();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SessionStatusBarDrawPanel(
      TStatusBar *StatusBar, TStatusPanel *Panel, const TRect &Rect)
{
  if (Terminal && Terminal->Active && Terminal->Status >= sshReady)
  {
    int ImageIndex;
    AnsiString PanelText;
    switch (Panel->Index - StatusBar->Tag) {
      case 2: ImageIndex = Terminal->SshVersion - 1; break;
      case 3: ImageIndex = 2 + (Terminal->CSCompression || Terminal->SCCompression); break;
      case 4: ImageIndex = 4; PanelText = CipherNames[Terminal->CSCipher]; break;
      default: assert(false); break;
    }
    TCanvas *SCanvas = StatusBar->Canvas;
    if ((Panel->Alignment == taCenter) && PanelText.IsEmpty())
    {
      NonVisualDataModule->SessionImages->Draw(SCanvas,
        Rect.Left + Rect.Width() / 2 - NonVisualDataModule->SessionImages->Width / 2,
        Rect.Top, ImageIndex, true);
    }
    else
    {
      NonVisualDataModule->SessionImages->Draw(SCanvas,
        Rect.Left + 1, Rect.Top, ImageIndex, true);
      if (!PanelText.IsEmpty())
      {
        SCanvas->TextOut(Rect.left + 18, Rect.top + 1, PanelText);
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SessionIdle()
{
  // terminal may not be active here, when connection is closed by remote side
  // and coresponding error message is being displayed
  if (Terminal && Terminal->Active)
  {
    Terminal->Idle();
  }
  UpdateStatusBar();
  FIgnoreNextSysCommand = false;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SessionStatusBarMouseMove(
      TObject * Sender, TShiftState /*Shift*/, int X, int Y)
{
  if (Terminal && Terminal->Active)
  {
    const int Hints[3] = { STATUS_RECEIVED_HINT, STATUS_SENT_HINT, STATUS_DURATION_HINT };
    TStatusBar * StatusBar = ((TStatusBar *)Sender);
    assert(StatusBar);
    TPoint Local(X, Y)/* = StatusBar->ScreenToClient(Mouse->CursorPos)*/;
    int IPanel = 0;
    while ((Local.x > StatusBar->Panels->Items[IPanel]->Width) &&
      (IPanel < StatusBar->Panels->Count - 1))
    {
      Local.x -= StatusBar->Panels->Items[IPanel]->Width;
      IPanel ++;
    }
    AnsiString AHint;
    if (StatusBar->Tag && (IPanel == 0))
    {
      AHint = LoadStr(STATUS_FILEINFO_HINT);
    }
    else
    {
      if (StatusBar->Tag) IPanel--;
      switch (IPanel) {
        case 0:
        case 1: AHint = LoadStr(Hints[IPanel]); break;
        case 6: AHint = LoadStr(Hints[2]); break;
        case 2: AHint = FMTLOAD(STATUS_VERSION_HINT, (Terminal->SshVersion)); break;

        case 3:
          if (Terminal->CSCompression == Terminal->SCCompression)
          {
            AHint = FMTLOAD(STATUS_COMPRESSION_HINT, (BooleanToStr(Terminal->CSCompression)));
          }
          else
          {
            AHint = FMTLOAD(STATUS_COMPRESSION2_HINT,
              (BooleanToStr(Terminal->CSCompression), BooleanToStr(Terminal->SCCompression)));
          }
          break;

        case 4:
          if (Terminal->CSCipher == Terminal->SCCipher)
          {
            AHint = FMTLOAD(STATUS_ENCRYPTION_HINT, (CipherNames[Terminal->CSCipher]));
          }
          else
          {
            AHint = FMTLOAD(STATUS_ENCRYPTION2_HINT,
              (CipherNames[Terminal->CSCipher], CipherNames[Terminal->SCCipher]));
          }
          break;

        case 5:
          AHint = FMTLOAD(STATUS_FS_PROTOCOL, (Terminal->ProtocolName));
          break;

        default: AHint = ""; break;
      }
    }

    if (AHint.IsEmpty())
    {
      StatusBar->Hint = AHint;
    }
    else
    {
      AHint = FORMAT("%s|X", (AHint));
      if (AHint != StatusBar->Hint)
      {
        Application->CancelHint();
        StatusBar->Hint = AHint;
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ApplicationHint(TObject * /*Sender*/)
{
  assert(GetComponent(fcStatusBar) && Application);
  AnsiString AHint = GetLongHint(Application->Hint);
  FShowStatusBarHint = Active && !AHint.IsEmpty() && (AHint != "X");
  if (FShowStatusBarHint)
  {
    FStatusBarHint = AHint != "E" ? AHint : AnsiString("");
  }
  else
  {
    FStatusBarHint = "";
  }
  UpdateStatusBar();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::NewSession()
{
  TSessionData * Data = new TSessionData("");
  try
  {
    Data->Assign(StoredSessions->DefaultSettings);
    if (DoLoginDialog(StoredSessions, Data, loAddSession))
    {
      assert(Data->CanLogin);
      TTerminalManager * Manager = TTerminalManager::Instance();
      TTerminal * Terminal = Manager->NewTerminal(Data);
      Manager->ActiveTerminal = Terminal;
      Manager->ConnectActiveTerminal();
    }
  }
  __finally
  {
    delete Data;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CloseSession()
{
  TTerminalManager::Instance()->FreeActiveTerminal();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::OpenStoredSession(TSessionData * Data)
{
  TTerminalManager * Manager = TTerminalManager::Instance();
  TTerminal * Terminal = Manager->NewTerminal(Data);
  Manager->ActiveTerminal = Terminal;
  Manager->ConnectActiveTerminal();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FormCloseQuery(TObject * /*Sender*/,
      bool &CanClose)
{
  if (Terminal->Active && WinConfiguration->ConfirmClosingSession)
  {
    int Result;
    if (TTerminalManager::Instance()->Count > 1)
    {
      Result = MessageDialog(LoadStr(CLOSE_SESSIONS), qtConfirmation,
        qaOK | qaCancel, 0, mpNeverAskAgainCheck);
    }
    else
    {
      Result = MessageDialog(FMTLOAD(CLOSE_SESSION,
        (Terminal->SessionData->SessionName)), qtConfirmation,
        qaOK | qaCancel, 0, mpNeverAskAgainCheck);
    }

    if (Result == qaNeverAskAgain)
    {
      WinConfiguration->ConfirmClosingSession = false;
    }
    CanClose = (Result == qaOK || Result == qaNeverAskAgain);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DropDownButtonMenu(TObject *Sender)
{
  ((TToolButton*)Sender)->CheckMenuDropdown();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteDirViewDisplayProperties(
      TObject *Sender)
{
  TStrings *FileList = ((TUnixDirView*)Sender)->CreateFileList(True, False, NULL);
  try
  {
    SetProperties(osRemote, FileList);
  }
  __finally
  {
    delete FileList;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SetComponentVisible(Word Component, Boolean value)
{
  TControl * Control = GetComponent((Word)(Component & 0x00FF));
  assert(Control);
  if ((dynamic_cast<TCoolBar*>(Control) != NULL) && (Component & 0xFF00))
  {
    TCoolBand * Band = GetCoolBand(dynamic_cast<TCoolBar*>(Control),
      (Component & 0xFF00) >> 8);
    assert(Band);
    Band->Visible = value;
  }
  else
  {
    Control->Visible = value;
  }

  if (RemoteDirView->ItemFocused != NULL)
  {
    RemoteDirView->ItemFocused->MakeVisible(false);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::GetComponentVisible(Word Component)
{
  TControl * Control = GetComponent((Word)(Component & 0x00FF));
  if (Control == NULL)
  {
    return false;
  }
  else if ((dynamic_cast<TCoolBar*>(Control) != NULL) && (Component & 0xFF00))
  {
    TCoolBand * Band = GetCoolBand(dynamic_cast<TCoolBar*>(Control),
      (Component & 0xFF00) >> 8);
    return Band != NULL ? Band->Visible : false;
  }
  else
  {
    return Control->Visible;
  }
}
//---------------------------------------------------------------------------
TCoolBand * __fastcall TCustomScpExplorerForm::GetCoolBand(TCoolBar * Coolbar, int ID)
{
  return dynamic_cast<TCoolBand *>(Coolbar->Bands->FindItemID(ID));
}
//---------------------------------------------------------------------------
TControl * __fastcall TCustomScpExplorerForm::GetComponent(Byte Component)
{
  switch (Component) {
    case fcStatusBar: return RemoteStatusBar;
    case fcCoolBar: return TopCoolBar;
    case fcRemotePopup: return reinterpret_cast<TControl *>(NonVisualDataModule->RemoteDirViewPopup);
    default: return NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DirViewColumnRightClick(
      TObject *Sender, TListColumn *Column, TPoint &Point)
{
  assert(NonVisualDataModule && Column && Sender);
  NonVisualDataModule->ListColumn = Column;
  TPoint ScreenPoint = ((TControl*)Sender)->ClientToScreen(Point);
  TPopupMenu * DirViewColumnMenu;
  if (Sender == RemoteDirView)
  {
    DirViewColumnMenu = NonVisualDataModule->RemoteDirViewColumnPopup;
  }
  else
  {
    DirViewColumnMenu = NonVisualDataModule->LocalDirViewColumnPopup;
  }
  DirViewColumnMenu->Popup(ScreenPoint.x, ScreenPoint.y);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DirViewExecFile(
      TObject *Sender, TListItem *Item, bool &AllowExec)
{
  DoDirViewExecFile(Sender, Item, AllowExec);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DoDirViewExecFile(TObject * Sender,
  TListItem * Item, bool & AllowExec)
{
  assert(Sender && Item && Configuration);
  TCustomDirView * ADirView = (TCustomDirView *)Sender;
  if (ADirView->ItemIsDirectory(Item))
  {
    AllowExec = true;
  }
  else if (WinConfiguration->CopyOnDoubleClick && !FForceExecution)
  {
    ExecuteFileOperation(foCopy,
      (ADirView == DirView(osRemote) ? osRemote : osLocal),
      true, !WinConfiguration->CopyOnDoubleClickConfirmation);
    AllowExec = false;
  }
  else if (ADirView == DirView(osRemote) && !WinConfiguration->DisableOpenEdit)
  {
    ExecuteFile(osRemote, efDefault);
    AllowExec = false;
  }
  else
  {
    AllowExec = true;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::GetHasDirView(TOperationSide Side)
{
  return ((Side == osRemote) || (Side == osCurrent));
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CompareDirectories()
{
  assert(false);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SynchronizeDirectories()
{
  assert(false);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FullSynchronizeDirectories()
{
  assert(false);
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::DoFullSynchronizeDirectories(
  AnsiString & LocalDirectory, AnsiString & RemoteDirectory, TSynchronizeMode & Mode)
{
  bool Result;
  int Params = GUIConfiguration->SynchronizeParams;

  bool SaveSettings;
  Result = DoFullSynchronizeDialog(Mode, Params, LocalDirectory, RemoteDirectory,
    SaveSettings);
  if (Result)
  {
    if (SaveSettings)
    {
      GUIConfiguration->SynchronizeParams = Params;
    }

    assert(!FAutoOperation);
    void * BatchStorage;
    BatchStart(BatchStorage);
    FAutoOperation = true;

    try
    {
      FSynchronizeProgressForm = new TSynchronizeProgressForm(Application);
      FSynchronizeProgressForm->Start();

      Terminal->Synchronize(LocalDirectory, RemoteDirectory,
        static_cast<TTerminal::TSynchronizeMode>(Mode),
        Params, TerminalSynchronizeDirectory);
    }
    __finally
    {
      FAutoOperation = false;
      SAFE_DESTROY(FSynchronizeProgressForm);
      BatchEnd(BatchStorage);
      if (HasDirView[osLocal])
      {
        DirView(osLocal)->Reload(true);
      }
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::TerminalSynchronizeDirectory(
  const AnsiString LocalDirectory, const AnsiString RemoteDirectory, bool & Continue)
{
  assert(FSynchronizeProgressForm != NULL);
  FSynchronizeProgressForm->SetData(LocalDirectory, RemoteDirectory, Continue);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ExploreLocalDirectory()
{
  assert(false);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SaveCurrentSession()
{
  AnsiString SessionName;
  SessionName = Terminal->SessionData->SessionName;
  SessionName = DoSaveSessionDialog(StoredSessions, SessionName);
  if (!SessionName.IsEmpty())
  {
    TSessionData * SessionData = new TSessionData("");
    try
    {
      SessionData->Assign(Terminal->SessionData);
      UpdateSessionData(SessionData);
      StoredSessions->NewSession(SessionName, SessionData);
      StoredSessions->Save();
    }
    __finally
    {
      delete SessionData;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateSessionData(TSessionData * Data)
{
  assert(Terminal && Terminal->SessionData);
  if (!Data)
  {
    Data = Terminal->SessionData;
  }

  // cannot use RemoteDirView->Path, because it is empty if connection
  // was already closed
  Data->RemoteDirectory = Terminal->CurrentDirectory;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ToolBarResize(TObject *Sender)
{
  TToolBar * ToolBar = (TToolBar*)Sender;
  TControl * Child = (TControl *)ToolBar->Controls[0];
  Child->Width = ToolBar->Width;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteDirViewWarnLackOfTempSpace(
      TUnixDirView * /*Sender*/, const AnsiString Path, __int64 RequiredSpace,
      bool &Continue)
{
  if (WinConfiguration->DDWarnLackOfTempSpace)
  {
    AnsiString ADrive = ExtractFileDrive(ExpandFileName(Path));
    if (!ADrive.IsEmpty())
    {
      __int64 FreeSpace = DiskFree((Byte)(ADrive[1]-'A'+1));
      Integer MessageRes = 0;
      if (RequiredSpace >= 0)
      {
        __int64 RequiredWithReserve;
        RequiredWithReserve = (__int64)(RequiredSpace * WinConfiguration->DDWarnLackOfTempSpaceRatio);
        if (FreeSpace < RequiredWithReserve) MessageRes = DD_WARN_LACK_OF_TEMP_SPACE;
      }
      else
      {
        MessageRes = DD_WARN_UNKNOWN_TEMP_SPACE;
      }

      if (MessageRes)
      {
        int Result;
        Result = MessageDialog(FMTLOAD(MessageRes, (Path,
          FormatBytes(FreeSpace), FormatBytes(RequiredSpace))),
          qtWarning, qaYes | qaNo, 0, mpNeverAskAgainCheck);

        if (Result == qaNeverAskAgain)
        {
          WinConfiguration->DDWarnLackOfTempSpace = false;
        }

        Continue = (Result == qaYes || Result == qaNeverAskAgain);
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::AddBookmark(TOperationSide Side)
{
  DoOpenDirectoryDialog(odAddBookmark, Side);
}
//---------------------------------------------------------------------------
TStrings * __fastcall TCustomScpExplorerForm::CreateVisitedDirectories(TOperationSide Side)
{
  TStringList * VisitedDirectories = new TStringList();
  try
  {
    TCustomDirView * DView = DirView(Side);

    VisitedDirectories->Duplicates = dupIgnore;
    // we should better use TCustomDirView::FCaseSensitive, but it is private
    VisitedDirectories->CaseSensitive = (Side == osRemote);
    VisitedDirectories->Sorted = true;

    for (int Index = -DView->BackCount; Index <= DView->ForwardCount; Index++)
    {
      VisitedDirectories->Add(DView->HistoryPath[Index]);
    }
  }
  catch (...)
  {
    delete VisitedDirectories;
    throw;
  }
  return VisitedDirectories;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DoOpenDirectoryDialog(
  TOpenDirectoryMode Mode, TOperationSide Side)
{
  if (Mode != odAddBookmark ||
      (MessageDialog(FMTLOAD(ADD_BOOKMARK_CONFIRM, (DirView(Side)->PathName)),
        qtConfirmation, qaYes | qaNo, 0) == qaYes))
  {
    TStrings * VisitedDirectories = CreateVisitedDirectories(Side);
    try
    {
      AnsiString Name = DirView(Side)->PathName;
      if (::DoOpenDirectoryDialog(Mode, Side, Name, VisitedDirectories, Terminal))
      {
        DirView(Side)->Path = Name;
      }
    }
    __finally
    {
      delete VisitedDirectories;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::OpenInPutty()
{
  OpenSessionInPutty(Terminal->SessionData);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::OpenConsole(AnsiString Command)
{
  DoConsoleDialog(Terminal, Command);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DirViewDDDragEnter(
      TObject *Sender, _di_IDataObject /*DataObj*/, int /*grfKeyState*/,
      const TPoint & /*Point*/, int & /*dwEffect*/, bool & Accept)
{
  if ((DropSourceControl == RemoteDirView) &&
      (FDDExtMapFile != NULL))
  {
    Accept = true;
  }

  FDDTargetDirView = (TCustomDirView*)Sender;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DirViewDDDragLeave(
      TObject *Sender)
{
  USEDPARAM(Sender);
  assert(FDDTargetDirView == Sender);
  FDDTargetDirView = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::AddEditLink()
{
  assert(FLastDirView == RemoteDirView);

  bool Edit = false;
  TRemoteFile * File = NULL;
  AnsiString FileName;
  AnsiString PointTo;
  bool SymbolicLink = true;

  if (RemoteDirView->ItemFocused)
  {
    assert(RemoteDirView->ItemFocused->Data);
    File = (TRemoteFile *)RemoteDirView->ItemFocused->Data;

    Edit = File->IsSymLink && Terminal->SessionData->ResolveSymlinks;
    if (Edit)
    {
      FileName = File->FileName;
      PointTo = File->LinkTo;
    }
    else
    {
      PointTo = File->FileName;
    }
  }

  if (DoSymlinkDialog(FileName, PointTo, osRemote, SymbolicLink, Edit,
        Terminal->IsCapable[fcHardLink]))
  {
    if (Edit)
    {
      assert(File->FileName == FileName);
      bool Recursive = false;
      Terminal->ExceptionOnFail = true;
      try
      {
        Terminal->DeleteFile("", File, &Recursive);
      }
      __finally
      {
        Terminal->ExceptionOnFail = false;
      }
    }
    Terminal->CreateLink(FileName, PointTo, SymbolicLink);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ExecuteCurrentFile()
{
  assert(!WinConfiguration->DisableOpenEdit);
  FForceExecution = true;
  try
  {
    DirView(osCurrent)->ExecuteCurrentFile();
  }
  __finally
  {
    FForceExecution = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::LastTerminalClosed(TObject * /*Sender*/)
{
  try
  {
    NewSession();
  }
  __finally
  {
    if (!Terminal || !Terminal->Active)
    {
      Application->Terminate();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::TerminalListChanged(TObject * /*Sender*/)
{
  TCustomCombo * SessionCombo = dynamic_cast<TCustomCombo *>(GetComponent(fcSessionCombo));
  if (SessionCombo != NULL)
  {
    SessionCombo->Items = TTerminalManager::Instance()->TerminalList;
    SessionCombo->ItemIndex = TTerminalManager::Instance()->ActiveTerminalIndex;
  }
  NonVisualDataModule->CreateOpenedSessionListMenu();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SessionComboDropDown(TObject * Sender)
{
  TCustomComboBox * SessionCombo = dynamic_cast<TCustomComboBox *>(Sender);
  assert(SessionCombo);
  TCanvas * Canvas = SessionCombo->Canvas;

  int MaxWidth = 0, Width;
  for (int i = 0; i < SessionCombo->Items->Count; i++)
  {
    Width = Canvas->TextExtent(SessionCombo->Items->Strings[i]).cx;
    TShortCut ShortCut = NonVisualDataModule->OpenSessionShortCut(i);
    if (ShortCut != scNone)
    {
      Width += Canvas->TextExtent(ShortCutToText(ShortCut)).cx;
    }
    if (Width > MaxWidth)
    {
      MaxWidth = Width;
    }
  }
  MaxWidth += 8;
  if (SessionCombo->Items->Count > ((TComboBox *)SessionCombo)->DropDownCount)
  {
    MaxWidth += GetSystemMetrics(SM_CXVSCROLL);
  }
  SessionCombo->Perform(CB_SETDROPPEDWIDTH, MaxWidth + 8 + 1, 0);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SessionComboDrawItem(
  TWinControl * Control, int Index, const TRect & Rect, TOwnerDrawState /*State*/)
{
  TCustomComboBox * SessionCombo = dynamic_cast<TCustomComboBox *>(Control);
  assert(SessionCombo);
  TCanvas * Canvas = SessionCombo->Canvas;
  Canvas->FillRect(Rect);

  if (Index >= 0)
  {
    int ShortCutWidth = 0;
    AnsiString ShortCutStr;
    if (Rect.Top != 3)
    {
      TShortCut ShortCut = NonVisualDataModule->OpenSessionShortCut(Index);
      if (ShortCut != scNone)
      {
        ShortCutStr = ShortCutToText(ShortCut);
        ShortCutWidth = Canvas->TextExtent(ShortCutStr).cx;
      }
    }

    TRect R = Rect;
    R.Right -= ShortCutWidth + 2;
    Canvas->TextRect(R, R.Left + 2, R.Top, SessionCombo->Items->Strings[Index]);
    R = Rect;
    R.Left = R.Right - ShortCutWidth - 2;
    Canvas->TextRect(R, R.Left, R.Top, ShortCutStr);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SessionComboChange(TObject * Sender)
{
  TCustomComboBox * SessionCombo = dynamic_cast<TCustomComboBox *>(Sender);
  assert(SessionCombo);
  TTerminal * Terminal;
  Terminal = dynamic_cast<TTerminal *>(SessionCombo->Items->Objects[SessionCombo->ItemIndex]);
  assert(Terminal);
  TTerminalManager::Instance()->ActiveTerminal = Terminal;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CMAppSysCommand(TMessage & Message)
{
  TWMSysCommand * SysCommand = (TWMSysCommand *)Message.LParam;
  if (SysCommand->CmdType != SC_KEYMENU || !FIgnoreNextSysCommand)
  {
    FIgnoreNextSysCommand = false;
    TForm::Dispatch(&Message);
  }
  else
  {
    Message.Result = 1;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DoShow()
{
  TToolBar * MenuToolBar = dynamic_cast<TToolBar*>(GetComponent(fcMenuToolBar));
  MenuToolBar->Height = MenuToolBar->Controls[0]->Height;

  TForm::DoShow();
}
//---------------------------------------------------------------------------
int __fastcall TCustomScpExplorerForm::MoreMessageDialog(const AnsiString Message,
    TStrings * MoreMessages, TQueryType Type, int Answers,
    int HelpCtx, int Params)
{
  if (WinConfiguration->ContinueOnError && (Params & mpAllowContinueOnError) &&
      FErrorList)
  {
    TStringList * MoreMessagesCopy = NULL;
    if (MoreMessages)
    {
      MoreMessagesCopy = new TStringList();
      MoreMessagesCopy->Assign(MoreMessages);
    }
    FErrorList->AddObject(Message, MoreMessagesCopy);
    if (Answers & qaSkip) return qaSkip;
      else
    if (Answers & qaIgnore) return qaIgnore;
      else
    if (Answers & qaOK) return qaOK;
      else
    if (Answers & qaYes) return qaYes;
      else
    if (Answers & qaRetry) return qaRetry;
      else
    {
      assert(false);
      return qaYes;
    }
  }
  else
  {
    return ::MoreMessageDialog(Message, MoreMessages, Type, Answers, HelpCtx, Params);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteDirViewDDCreateDragFileList(
  TObject * /*Sender*/, TFileList * FileList, bool & Created)
{
  if (FDDExtMapFile != NULL)
  {
    CloseHandle(FDDExtMapFile);
    FDDExtMapFile = NULL;
  }

  if (WinConfiguration->DDExtEnabled)
  {
    DDExtInitDrag(FileList, Created);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DDExtInitDrag(TFileList * FileList,
  bool & Created)
{
  FDragExtFakeDirectory =
    ExcludeTrailingBackslash(UniqTempDir(WinConfiguration->DDTemporaryDirectory));
  ForceDirectories(FDragExtFakeDirectory);
  FileList->AddItem(NULL, FDragExtFakeDirectory);

  Created = true;

  FDDExtMapFile = CreateFileMapping((HANDLE)0xFFFFFFFF, NULL, PAGE_READWRITE,
    0, sizeof(TDragExtCommStruct), DRAG_EXT_MAPPING);

  {
    TMutexGuard Guard(FDDExtMutex, DRAGEXT_MUTEX_RELEASE_TIMEOUT);
    TDragExtCommStruct* CommStruct;
    CommStruct = static_cast<TDragExtCommStruct*>(MapViewOfFile(FDDExtMapFile,
      FILE_MAP_ALL_ACCESS, 0, 0, 0));
    assert(CommStruct != NULL);
    CommStruct->Version = TDragExtCommStruct::CurrentVersion;
    CommStruct->Dragging = true;
    strncpy(CommStruct->DropDest, FDragExtFakeDirectory.c_str(),
      sizeof(CommStruct->DropDest));
    CommStruct->DropDest[sizeof(CommStruct->DropDest) - 1] = '\0';
    UnmapViewOfFile(CommStruct);
  }

  FDDExtDropEffect = DROPEFFECT_NONE;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteDirViewDDEnd(TObject * /*Sender*/)
{
  if (FDDExtMapFile != NULL)
  {
    try
    {
      if (FDDExtDropEffect != DROPEFFECT_NONE)
      {
        AnsiString TargetDirectory;

        DDGetTarget(TargetDirectory);

        ExecuteFileOperation(
          FDDExtDropEffect == DROPEFFECT_MOVE ? foMove : foCopy, osRemote, true,
          !WinConfiguration->DDTransferConfirmation, &TargetDirectory);
      }
    }
    __finally
    {
      CloseHandle(FDDExtMapFile);
      FDDExtMapFile = NULL;
      RemoveDir(FDragExtFakeDirectory);
      FDragExtFakeDirectory = "";
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DDGetTarget(AnsiString & Directory)
{
  bool Result = false;

  Enabled = false;
  try
  {
    int Timer = 0;
    while (!Result && (Timer < WinConfiguration->DDExtTimeout))
    {
      {
        TMutexGuard Guard(FDDExtMutex, DRAGEXT_MUTEX_RELEASE_TIMEOUT);
        TDragExtCommStruct* CommStruct;
        CommStruct = static_cast<TDragExtCommStruct*>(MapViewOfFile(FDDExtMapFile,
          FILE_MAP_ALL_ACCESS, 0, 0, 0));
        assert(CommStruct != NULL);
        Result = !CommStruct->Dragging;
        if (Result)
        {
          Directory = ExtractFilePath(CommStruct->DropDest);
        }
        UnmapViewOfFile(CommStruct);
      }
      Sleep(50);
      Timer += 50;
      Application->ProcessMessages();
    }
  }
  __finally
  {
    Enabled = true;
  }

  if (!Result)
  {
    throw Exception(LoadStr(DRAGEXT_TARGET_UNKNOWN));
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteDirViewDDTargetDrop(
  TUnixDirView * /*Sender*/, int DropEffect, bool & Continue)
{
  if ((DropEffect == DROPEFFECT_MOVE) &&
      (FDDTargetDirView == RemoteDirView))
  {
    ExecuteFileOperation(foRemoteMove, osRemote, true,
      !WinConfiguration->DDTransferConfirmation);
    // abort drag&drop
    Abort();
  }
  else if (FDDExtMapFile != NULL)
  {
    Continue = false;
    FDDExtDropEffect = DropEffect;
  }
}
//---------------------------------------------------------------------------
class TFakeDataObjectFilesEx : public TDataObjectFilesEx
{
public:
	__fastcall TFakeDataObjectFilesEx(TFileList * AFileList, bool RenderPIDL,
    bool RenderFilename) : TDataObjectFilesEx(AFileList, RenderPIDL, RenderFilename)
  {
  }

  virtual bool __fastcall AllowData(const tagFORMATETC & FormatEtc)
  {
    return (FormatEtc.cfFormat == CF_HDROP) ? false :
      TDataObjectFilesEx::AllowData(FormatEtc);
  }
};
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteDirViewDDCreateDataObject(
  TObject * /*Sender*/, TDataObject *& DataObject)
{
  if (FDDExtMapFile != NULL)
  {
    TFileList * FileList = RemoteDirView->DragDropFilesEx->FileList;
    if (!FileList->RenderPIDLs() || !FileList->RenderNames())
    {
      Abort();
    }

    if (FileList->Count > 0)
    {
      TDataObjectFilesEx * FilesObject = new TFakeDataObjectFilesEx(FileList, true, true);
      if (!FilesObject->IsValid(true, true))
      {
        FilesObject->_Release();
      }
      else
      {
        DataObject = FilesObject;
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::GoToCommandLine()
{
  assert(false);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::PanelExport(TOperationSide Side,
  TPanelExport Export, TPanelExportDestination Destination, bool OnFocused)
{
  Side = GetSide(Side);

  TCustomDirView * DirView = this->DirView(Side);
  TStringList * ExportData = new TStringList();
  try
  {
    if (Export == pePath)
    {
      ExportData->Add(DirView->PathName);
    }
    else
    {
      bool FullPath = (Export == peFullFileList);
      DirView->CreateFileList(OnFocused, FullPath, ExportData);
      AnsiString FileName;
      for (int Index = 0; Index < ExportData->Count; Index++)
      {
        if (ExportData->Strings[Index].Pos(" ") > 0)
        {
          ExportData->Strings[Index] = FORMAT("\"%s\"", (ExportData->Strings[Index]));
        }
      }
    }

    PanelExportStore(Side, Export, Destination, ExportData);
  }
  __finally
  {
    delete ExportData;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::PanelExportStore(TOperationSide /*Side*/,
  TPanelExport /*Export*/, TPanelExportDestination Destination,
  TStringList * ExportData)
{
  if (Destination == pedClipboard)
  {
    Clipboard()->AsText = ExportData->Text;
  }
  else
  {
    assert(false);
  }
}
