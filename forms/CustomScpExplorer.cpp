//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "CustomScpExplorer.h"

#include <Common.h>
#include <Interface.h>
#include <Net.h>
#include <ScpMain.h>
#include <TextsWin.h>
#include <DiscMon.hpp>

#include <VCLCommon.h>
#include <Log.h>

#include "NonVisual.h"
#include "Tools.h"
#include <Progress.h>
#include <OperationStatus.h>
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

  UseSystemFont(this);
}
//---------------------------------------------------------------------------
__fastcall TCustomScpExplorerForm::~TCustomScpExplorerForm()
{
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
      if (Terminal->OnProgress == OperationProgress) Terminal->OnProgress = NULL;
      if (Terminal->OnFinished == OperationFinished) Terminal->OnFinished = NULL;
    }
    FTerminal = value;
    if (Terminal)
    {
      Terminal->OnProgress = OperationProgress;
      Terminal->OnFinished = OperationFinished;
    }
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
  NonVisualDataModule->SessionIdleTimer->Enabled = (Terminal != NULL);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ConfigurationChanged()
{
  assert(Configuration && RemoteDirView);
  RemoteDirView->DDAllowMove = Configuration->DDAllowMove;
  RemoteDirView->DimmHiddenFiles = Configuration->DimmHiddenFiles;
  RemoteDirView->ShowHiddenFiles = Configuration->ShowHiddenFiles;
  RemoteDirView->ShowInaccesibleDirectories = Configuration->ShowInaccesibleDirectories;
  RemoteDirView->DDTemporaryDirectory = Configuration->DDTemporaryDirectory;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteDirViewGetCopyParam(
      TUnixDirView * /*Sender*/, TTransferDirection Direction,
      TTransferType Type, AnsiString &TargetDirectory, TStrings * FileList,
      TCopyParamType &CopyParam)
{
  if (!CopyParamDialog(Direction, Type, true, FileList,
        TargetDirectory, CopyParam, Configuration->DDTransferConfirmation))
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
  assert(Terminal && Terminal->Active);
  if (Confirm)
  {
    return DoCopyDialog(Direction, Type, DragDrop, FileList,
      (Terminal->SessionData->EOLType != Configuration->LocalEOLType),
      TargetDirectory, &CopyParam);
  }
  else
  {
    return true;
  }
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
    // When main window is hidden, we suppose "/upload" mode
    if (!Visible)
    {
      FProgressForm->DisconnectWhenComplete = true;
    }
  }
  // operation is finished (or terminated), so we hide progress form
  else if (!ProgressData.InProgress && FProgressForm)
  {
    try
    {
      delete FProgressForm;
    }
    __finally
    {
      FProgressForm = NULL;
    }
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
void __fastcall TCustomScpExplorerForm::DoOperationFinished(TOperationSide Side,
  bool DragDrop, const AnsiString FileName, bool Success,
  bool & DisconnectWhenComplete)
{
  if (!FAutoOperation)
  {
    // no selection on "/upload", form servers only as event handler
    // (it is not displayed)
    if (!DragDrop && Visible)
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
void __fastcall TCustomScpExplorerForm::OperationFinished(TOperationSide Side,
  bool DragDrop, const AnsiString FileName, Boolean Success,
  bool & DisconnectWhenComplete)
{
  DoOperationFinished(Side, DragDrop, FileName, Success, DisconnectWhenComplete);
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

    NonVisualDataModule->CurrentOpenMenuItem->Default = !Configuration->CopyOnDoubleClick;
    NonVisualDataModule->CurrentCopyMenuItem->Default = Configuration->CopyOnDoubleClick;
    NonVisualDataModule->CurrentOpenMenuItem->Visible = Configuration->ExpertMode;
    NonVisualDataModule->CurentEditMenuItem->Visible = Configuration->ExpertMode;
    
    NonVisualDataModule->RemoteDirViewPopup->Popup(ScreenPoint.x, ScreenPoint.y);
  }
  Handled = true;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ExecuteFileOperation(TFileOperation Operation,
  TOperationSide Side, bool OnFocused, bool NoConfirmation)
{
  if (Side == osCurrent)
  {
    if (FLastDirView == RemoteDirView)
    {
      Side = osRemote;
    }
    else
    {
      assert(FLastDirView);
      Side = osLocal;
    }
  }
  bool PrevWatchForChanges = (HasDirView[osLocal] ? DirView(osLocal)->WatchForChanges : false);

  TStrings *FileList = DirView(Side)->CreateFileList(OnFocused, (Side == osLocal), NULL);
  try
  {
    if ((Operation == foCopy) || (Operation == foMove))
    {
      TTransferDirection Direction = (Side == osLocal ? tdToRemote : tdToLocal);
      TTransferType Type = (Operation == foCopy ? ttCopy : ttMove);
      AnsiString TargetDirectory;
      TCopyParamType CopyParam = Configuration->CopyParam;
      if (CopyParamDialog(Direction, Type, false, FileList, TargetDirectory,
          CopyParam, !NoConfirmation))
      {
        assert(Terminal);
        if (Side == osLocal)
        {
          TCustomDirView * DView = DirView(osLocal);
          SAVE_SELECTION(DView);
          int Params = 0;
          if (Operation == foMove) Params |= cpDelete;
          Terminal->CopyToRemote(FileList, TargetDirectory, &CopyParam, Params);
          if (Operation == foMove)
          {
            DView->Reload(True);
            RESTORE_SELECTION(DView);
          }
        }
        else
        {
          SAVE_SELECTION(DirView(osRemote));

          if (HasDirView[osLocal])
          {
            DirView(osLocal)->WatchForChanges = false;
          }

          try
          {
            Terminal->CopyToLocal(FileList, TargetDirectory, &CopyParam,
              (Operation == foMove ? cpDelete : 0));
          }
          __finally
          {
            if (Operation == foMove)
            {
              RESTORE_SELECTION(DirView(osRemote));
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
    }
    else if (Operation == foRename)
    {
      assert(DirView(Side)->ItemFocused);
      DirView(Side)->ItemFocused->EditCaption();
    }
    else if (Operation == foDelete)
    {
      assert(FileList->Count);
      bool Confirmed = !Configuration->ConfirmDeleting;
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
          Configuration->ConfirmDeleting = false;
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
    else
    {
      assert(false);
    }
  }
  __finally
  {
    delete FileList;
    if (HasDirView[osLocal])
    {
      DirView(osLocal)->WatchForChanges = PrevWatchForChanges;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ExecuteFile(TOperationSide Side,
  TExecuteFileBy ExecuteFileBy)
{
  if (Side == osCurrent)
  {
    Side = FLastDirView == RemoteDirView ? osRemote : osLocal;
  }

  bool Edit = (ExecuteFileBy == efEditor || ExecuteFileBy == efAlternativeEditor);

  TStrings * FileList = FileList = DirView(Side)->CreateFileList(true, Side == osLocal);
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

      AnsiString TempDir = UniqTempDir(Configuration->DDTemporaryDirectory);
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

      if (Edit && ((Configuration->Editor.Editor == edInternal) !=
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
              if (ExecuteShellAndWait(Configuration->Editor.ExternalEditor,
                    FExecutedFile) < 0)
              {
                throw Exception(FMTLOAD(EDITOR_ERROR, (Configuration->Editor.ExternalEditor)));
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
        if (!DeleteFile(FileName) || !RemoveDir(ExtractFilePath(FileName)))
        {
          throw Exception(FMTLOAD(DELETE_TEMP_EXECUTE_FILE_ERROR,
            (ExtractFilePath(FileName))));
        }
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
        if (FileTimestamp > FExecutedFileTimestamp)
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
              Terminal->CopyToRemote(FileList, RemoteDirView->PathName, &CopyParam, 0);
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
  FLastDirView = ((TCustomDirView *)Sender);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DeleteFiles(TOperationSide Side,
  TStrings * FileList)
{
  assert(Terminal);
  TCustomDirView *DView = DirView(Side);
  SAVE_SELECTION(DView);

  if (Side == osRemote)
  {
    Terminal->DeleteFiles(FileList);
  }
  else
  {
    TFileOperationProgressType Progress(&OperationProgress, &OperationFinished);
    bool DisconnectWhenComplete = false;

    try
    {
      Progress.Start(foDelete, Side, FileList->Count);
      try
      {
        int Index = 0;
        while ((Index < FileList->Count) && (Progress.Cancel == csContinue))
        {
          Progress.SetFile(FileList->Strings[Index]);
          AnsiString OnlyFileName = (Side == osLocal ?
            ExtractFileName(Progress.FileName) : UnixExtractFileName(Progress.FileName));
          try
          {
            if (!FileOperatorDelete(Progress.FileName, Configuration->DeleteToRecycleBin))
            {
              throw Exception(FMTLOAD(DELETE_LOCAL_FILE_ERROR, (Progress.FileName)));
            }
            Progress.Finish(OnlyFileName, true, DisconnectWhenComplete);
          }
          catch (EAbort & E)
          {
            break;
          }
          catch (Exception & E)
          {
            int Result = ExceptionMessageDialog(&E, qtError, qaRetry | qaIgnore | qaAbort);
            if (Result == qaRetry)
            {
              Index--;
            }
            else
            {
              Progress.Finish(OnlyFileName, false, DisconnectWhenComplete);
              if (Result == qaAbort)
              {
                break;
              }
            }
          }
          Index++;
        }
      }
      __finally
      {
        Progress.Stop();
      }
    }
    __finally
    {
      DView->Reload(True);
    }

    if (DisconnectWhenComplete && (Progress.Cancel == csContinue))
    {
      Terminal->CloseOnCompletion();
    }
  }
  RESTORE_SELECTION(DView);
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

    for (Integer Index = 0; Index < FileList->Count; Index++)
    {
      TRemoteFile * File = (TRemoteFile *)(FileList->Objects[Index]);
      assert(File);
      if (!Index)
      {
        CurrentProperties.Rights = *(File->Rights);
        CurrentProperties.Rights.AllowUndef = File->IsDirectory || File->Rights->IsUndef;
        CurrentProperties.Owner = File->Owner;
        CurrentProperties.Group = File->Group;
        CurrentProperties.Valid = TValidProperties() << vpRights << vpGroup << vpOwner;
      }
      else
      {
        CurrentProperties.Rights.AllowUndef = True;
        CurrentProperties.Rights &= *File->Rights;
        if (CurrentProperties.Owner != File->Owner)
        {
          CurrentProperties.Owner = "";
          CurrentProperties.Valid >> vpOwner;
        };
        if (CurrentProperties.Group != File->Group)
        {
          CurrentProperties.Group = "";
          CurrentProperties.Valid >> vpGroup;
        };
      }
    }

    int Flags = 0;
    if (Terminal->IsCapable[fcModeChanging]) Flags |= cpMode;
    if (Terminal->IsCapable[fcOwnerChanging]) Flags |= cpOwner;
    if (Terminal->IsCapable[fcGroupChanging]) Flags |= cpGroup;

    TRemoteProperties NewProperties = CurrentProperties;
    if (DoPropertiesDialog(FileList, RemoteDirView->PathName,
        Terminal->UserGroups, &NewProperties, Flags))
    {
      if (!NewProperties.Recursive)
      {
        if (NewProperties.Rights == CurrentProperties.Rights &&
            !NewProperties.AddXToDirectories) NewProperties.Valid >> vpRights;
        if (NewProperties.Group == CurrentProperties.Group)
          NewProperties.Valid >> vpGroup;
        if (NewProperties.Owner == CurrentProperties.Owner)
          NewProperties.Valid >> vpOwner;
      }

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
    for (Integer Index = 0; Index < NonVisualDataModule->ExplorerActions->ActionCount; Index++)
    {
      TAction * Action = (TAction *)NonVisualDataModule->ExplorerActions->Actions[Index];
      if ((Action->ShortCut == KeyShortCut) && AllowedAction(Action, aaShortCut))
      {
        Key = 0;
        Action->Execute();
        return;
      }
    }
  }

  TForm::KeyDown(Key, Shift);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateStatusBar()
{
  TStatusBar * SessionStatusBar = (TStatusBar *)GetComponent(fcStatusBar);
  assert(SessionStatusBar && (SessionStatusBar->Panels->Count >= 3) && Terminal);
  Integer Index = SessionStatusBar->Tag;
  SessionStatusBar->Panels->Items[Index]->Text = FormatBytes(Terminal->BytesReceived);
  SessionStatusBar->Panels->Items[Index + 1]->Text = FormatBytes(Terminal->BytesSent);
  SessionStatusBar->Panels->Items[Index + 5]->Text = Terminal->ProtocolName;
  SessionStatusBar->Panels->Items[Index + 6]->Text =
    FormatDateTime(Configuration->TimeFormat, Terminal->Duration);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SessionStatusBarDrawPanel(
      TStatusBar *StatusBar, TStatusPanel *Panel, const TRect &Rect)
{
  if (Terminal && Terminal->Active)
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
  assert(Terminal);
  // terminal may not be active here, when connection is closed by remote side
  // and coresponding error message is being displayed
  if (Terminal->Active)
  {
    UpdateStatusBar();
    Terminal->Idle();
  }
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
  TStatusBar * SessionStatusBar = (TStatusBar *)GetComponent(fcStatusBar);
  assert(SessionStatusBar && Application);
  AnsiString AHint = GetLongHint(Application->Hint);
  bool Show = Active && !AHint.IsEmpty() && (AHint != "X");
  if (Show)
  {
    SessionStatusBar->SimpleText = AHint != "E" ? AHint : AnsiString("");
    SessionStatusBar->SimplePanel = true;
  }
  else
  {
    SessionStatusBar->SimplePanel = false;
    SessionStatusBar->Invalidate();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::NewSession()
{
  if (!ExecuteShell(Application->ExeName, ""))
  {
    throw Exception(NEWSESSION_ERROR);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::OpenStoredSession(TSessionData * Data)
{
  assert(Data && StoredSessions && (StoredSessions->IndexOf(Data) >= 0));
  if (!ExecuteShell(Application->ExeName, FORMAT("\"%s\"", (Data->Name))))
  {
    throw Exception(FMTLOAD(LOADSESSION_ERROR, (Data->Name)));
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SaveSessionData(TSessionData * aSessionData)
{
  aSessionData->Assign(Terminal->SessionData);
  aSessionData->RemoteDirectory = RemoteDirView->Path;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SaveCurrentSession()
{
  AnsiString SessionName;
  SessionName = Terminal->SessionData->Name;
  SessionName = DoSaveSessionDialog(StoredSessions, SessionName);
  if (!SessionName.IsEmpty())
  {
    TSessionData * SessionData = new TSessionData("");
    try
    {
      SaveSessionData(SessionData);
      StoredSessions->NewSession(SessionName, SessionData);
    }
    __finally
    {
      delete SessionData;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FormCloseQuery(TObject * /*Sender*/,
      bool &CanClose)
{
  if (Terminal->Active && Configuration->ConfirmClosingSession)
  {
    int Result;
    Result = MessageDialog(FMTLOAD(CLOSE_SESSION,
      (Terminal->SessionData->SessionName)), qtConfirmation,
      qaOK | qaCancel, 0, mpNeverAskAgainCheck);
      
    if (Result == qaNeverAskAgain)
    {
      Configuration->ConfirmClosingSession = false;
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
  if (Control->InheritsFrom(__classid(TCoolBar)) && (Component & 0xFF00))
  {
    TCoolBand * Band = ((TCoolBand *)(((TCoolBar*)Control)->Bands->
      FindItemID((Component & 0xFF00) >> 8)));
    assert(Band);
    Band->Visible = value;
  }
  else
  {
    Control->Visible = value;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::GetComponentVisible(Word Component)
{
  TControl * Control = GetComponent((Word)(Component & 0x00FF));
  assert(Control);
  if (Control->InheritsFrom(__classid(TCoolBar)) && (Component & 0xFF00))
  {
    TCoolBand * Band = ((TCoolBand *)(((TCoolBar*)Control)->Bands->
      FindItemID((Component & 0xFF00) >> 8)));
    return Band ? Band->Visible : false;
  }
  else
  {
    return Control->Visible;
  }
}
//---------------------------------------------------------------------------
TControl * __fastcall TCustomScpExplorerForm::GetComponent(Byte Component)
{
  switch (Component) {
    case fcStatusBar: return RemoteStatusBar;
    case fcCoolBar: return TopCoolBar;
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
  else if (Configuration->CopyOnDoubleClick && !FForceExecution)
  {
    ExecuteFileOperation(foCopy,
      (ADirView == DirView(osRemote) ? osRemote : osLocal),
      true, !Configuration->CopyOnDoubleClickConfirmation);
    AllowExec = false;
  }
  else if (ADirView == DirView(osRemote))
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
void __fastcall TCustomScpExplorerForm::ExploreLocalDirectory()
{
  assert(false);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateSessionData(TSessionData * Data)
{
  assert(Terminal && Terminal->SessionData);
  if (!Data)
  {
    Data = Terminal->SessionData;
  }
  if (Data->UpdateDirectories || (Data != Terminal->SessionData))
  {
    Data->RemoteDirectory = Terminal->Files->Directory;
  }
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
  if (Configuration->DDWarnLackOfTempSpace)
  {
    AnsiString ADrive = ExtractFileDrive(ExpandFileName(Path));
    if (!ADrive.IsEmpty())
    {
      __int64 FreeSpace = DiskFree((Byte)(ADrive[1]-'A'+1));
      Integer MessageRes = 0;
      if (RequiredSpace >= 0)
      {
        __int64 RequiredWithReserve;
        RequiredWithReserve = (__int64)(RequiredSpace * Configuration->DDWarnLackOfTempSpaceRatio);
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
          Configuration->DDWarnLackOfTempSpace = false;
        }

        Continue = (Result == qaYes || Result == qaNeverAskAgain);
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::AddBookmark(TOperationSide Side)
{
  if (MessageDialog(FMTLOAD(ADD_BOOKMARK_CONFIRM, (DirView(Side)->PathName)),
        qtConfirmation, qaYes | qaNo, 0) == qaYes)
  {
    DoOpenDirectoryDialog(odAddBookmark, Side);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DoOpenDirectoryDialog(TOpenDirectoryMode Mode,
  TOperationSide Side, const AnsiString Bookmark)
{
  TStringList * VisitedDirectories;
  VisitedDirectories = new TStringList();
  try
  {
    TCustomDirView * DView = DirView(Side);

    VisitedDirectories->Duplicates = dupIgnore;
    // we should better use TCustomDirView::FCaseSensitive, but it is private
    VisitedDirectories->CaseSensitive = (Side == osRemote);
    VisitedDirectories->Sorted = True;

    for (int Index = -DView->BackCount; Index <= DView->ForwardCount; Index++)
    {
      VisitedDirectories->Add(DView->HistoryPath[Index]);
    }

    AnsiString Name = DView->PathName;
    if (OpenDirectoryDialog(Mode, Side, Name, VisitedDirectories, Terminal))
    {
      DirView(Side)->Path = Name;
    }
  }
  __finally
  {
    delete VisitedDirectories;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::OpenConsole()
{
  DoConsoleDialog(Terminal);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DirViewDDDragEnter(
      TObject *Sender, _di_IDataObject /*DataObj*/, int /*grfKeyState*/,
      const TPoint & /*Point*/, int &/*dwEffect*/, bool & /*Accept*/)
{
  FDDTargetDirView = (TCustomDirView*)Sender;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DirViewDDDragLeave(
      TObject *Sender)
{
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

    Edit = File->IsSymLink;
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

