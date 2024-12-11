//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <WinInterface.h>
#include <VCLCommon.h>
#include <TextsWin.h>
#include <WinConfiguration.h>
#include <CoreMain.h>
#include <Tools.h>
#include <BaseUtils.hpp>
#include <Terminal.h>
#include "FileFind.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "HistoryComboBox"
#pragma link "IEListView"
#pragma link "NortonLikeListView"
#pragma link "PngImageList"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
TFileFindDialog * FileFindDialog = NULL;
static const int NoSort = 1;
//---------------------------------------------------------------------------
void __fastcall ShowFileFindDialog(
  TTerminal * Terminal, UnicodeString Directory, TFindEvent OnFind, TFocusFileEvent OnFocusFile,
  TFileListOperationEvent OnDeleteFiles, TFileListOperationEvent OnDownloadFiles, TFileListOperationEvent OnEditFiles)
{
  if (FileFindDialog == NULL)
  {
    FileFindDialog = new TFileFindDialog(Application);
  }
  FileFindDialog->Init(Terminal, Directory, OnFind, OnFocusFile, OnDeleteFiles, OnDownloadFiles, OnEditFiles);
  FileFindDialog->Show();
}
//---------------------------------------------------------------------------
void __fastcall HideFileFindDialog()
{
  if (FileFindDialog != NULL)
  {
    delete FileFindDialog;
  }
}
//---------------------------------------------------------------------------
__fastcall TFileFindDialog::TFileFindDialog(TComponent * Owner)
  : TForm(Owner)
{
  UseSystemSettings(this);
  FState = ffInit;

  FixComboBoxResizeBug(MaskEdit);
  FixComboBoxResizeBug(RemoteDirectoryEdit);
  HintLabel(MaskHintText,
    FORMAT(L"%s\n \n%s\n \n%s\n \n%s\n \n%s\n \n%s", (LoadStr(MASK_HINT2),
      LoadStr(FILE_MASK_EX_HINT), LoadStr(COMBINING_MASKS_HINT),
      LoadStr(PATH_MASK_HINT2), LoadStr(DIRECTORY_MASK_HINT),
      LoadStr(MASK_HELP))));

  UpdateImages();

  UseDesktopFont(FileView);
  UseDesktopFont(StatusBar);

  FFrameAnimation.Init(AnimationPaintBox, L"Find");
  FixFormIcons(this);
}
//---------------------------------------------------------------------------
__fastcall TFileFindDialog::~TFileFindDialog()
{
  TFindFileConfiguration FormConfiguration = CustomWinConfiguration->FindFile;
  // | is there to make TCustomListViewColProperties.SetParamsStr stop there
  // (and it allows adding new parameters to the col properties in the future)
  FormConfiguration.ListParams = FORMAT(L"%s|/%d", (FileView->ColProperties->ParamsStr, 1));
  UnicodeString WindowParams = StoreFormSize(this);
  // this is particularly to prevent saving the form state
  // for the first time, keeping default positioning by a system
  if (!FWindowParams.IsEmpty() && (FWindowParams != WindowParams))
  {
    FormConfiguration.WindowParams = WindowParams;
  }
  CustomWinConfiguration->FindFile = FormConfiguration;

  Clear();
  DebugAssert(FileFindDialog == this);
  FileFindDialog = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::UpdateImages()
{
  FileView->SmallImages = ShellImageListForControl(this, ilsSmall);
}
//---------------------------------------------------------------------------
bool __fastcall TFileFindDialog::IsFinding()
{
  return (FState == ffFinding) || (FState == ffAborting);
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::UpdateControls()
{
  bool Finding = IsFinding();
  Caption = LoadStr(Finding ? FIND_FILE_FINDING : FIND_FILE_TITLE) + TitleSeparator + FTerminalName;
  UnicodeString StartStopCaption;
  if (Finding)
  {
    EnableControl(StartStopButton, true);
    StartStopCaption = LoadStr(FIND_FILE_STOP);
  }
  else
  {
    EnableControl(StartStopButton, !RemoteDirectoryEdit->Text.IsEmpty());
    StartStopCaption = LoadStr(FIND_FILE_START);
  }
  StartStopButton->Caption = StartStopCaption;
  EnableControl(FilterGroup, !Finding);
  FocusAction->Enabled = (FileView->ItemFocused != NULL);
  bool EnableFileOperations = !Finding && (FileView->SelCount > 0);
  DeleteAction->Enabled = EnableFileOperations;
  DownloadAction->Enabled = EnableFileOperations;
  EditAction->Enabled = EnableFileOperations;
  CopyAction->Enabled = (FileView->Items->Count > 0);
  SelectAllAction->Enabled = (FileView->SelCount < FileView->Items->Count);

  switch (FState)
  {
    case ffInit:
      StatusBar->SimpleText = L"";
      break;

    case ffFinding:
    case ffAborting:
      if (!FFindingInDirectory.IsEmpty())
      {
        StatusBar->SimpleText = FMTLOAD(FIND_FILE_IN_DIRECTORY, (FFindingInDirectory));
      }
      else
      {
        StatusBar->SimpleText = L"";
      }
      break;

    case ffAborted:
      StatusBar->SimpleText = LoadStr(FIND_FILE_ABORTED);
      break;

    case ffDone:
      StatusBar->SimpleText = LoadStr(FIND_FILE_DONE);
      break;

    default:
      DebugFail();
      break;
  }

  FocusButton->Default = FileView->Focused() && (FState != ffInit);
  StartStopButton->Default = !FocusButton->Default;
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::ControlChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::Init(
  TTerminal * Terminal, UnicodeString Directory, TFindEvent OnFind, TFocusFileEvent OnFocusFile,
  TFileListOperationEvent OnDeleteFiles, TFileListOperationEvent OnDownloadFiles, TFileListOperationEvent OnEditFiles)
{
  if (FTerminal != Terminal)
  {
    FTerminal = Terminal;
    FTerminalName = Terminal->SessionData->SessionName;
    Clear();
    FState = ffInit;
    ActiveControl = MaskEdit;
  }

  FOnFind = OnFind;
  FOnFocusFile = OnFocusFile;
  FOnDeleteFiles = OnDeleteFiles;
  FOnDownloadFiles = OnDownloadFiles;
  FOnEditFiles = OnEditFiles;

  MaskEdit->Text = WinConfiguration->SelectMask;
  RemoteDirectoryEdit->Text = UnixExcludeTrailingBackslash(Directory);
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::CreateParams(TCreateParams & Params)
{
  TForm::CreateParams(Params);
  Params.WndParent = GetDesktopWindow();
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::ClearItem(TListItem * Item)
{
  TRemoteFile * File = static_cast<TRemoteFile *>(Item->Data);
  Item->Data = NULL;
  delete File;
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::Clear()
{
  for (int Index = 0; Index < FileView->Items->Count; Index++)
  {
    ClearItem(FileView->Items->Item[Index]);
  }

  FileView->Items->Clear();
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::Start()
{
  if (MaskEdit->Focused())
  {
    MaskEditExit(NULL);
  }

  RemoteDirectoryEdit->SaveToHistory();
  CustomWinConfiguration->History[L"RemoteDirectory"] = RemoteDirectoryEdit->Items;
  MaskEdit->SaveToHistory();
  WinConfiguration->History[L"Mask"] = MaskEdit->Items;
  WinConfiguration->SelectMask = MaskEdit->Text;

  DebugAssert(FState != ffFinding);

  FState = ffFinding;
  FClosePending = false;
  try
  {
    FFrameAnimation.Start();
    UpdateControls();
    Repaint();

    TOperationVisualizer Visualizer;

    DebugAssert(FOnFind != NULL);
    UnicodeString Directory = UnixExcludeTrailingBackslash(RemoteDirectoryEdit->Text);

    FDirectory = Directory;
    if (FDirectory == ROOTDIRECTORY)
    {
      FDirectory = UnicodeString();
    }

    FOnFind(FTerminal, Directory, MaskEdit->Text, FileFound, FindingFile);
  }
  __finally
  {
    FFindingInDirectory = L"";
    if (FState == ffFinding)
    {
      FState = ffDone;
    }
    if (FState == ffAborting)
    {
      FState = ffAborted;
    }
    FFrameAnimation.Stop();
    if (WindowState == wsMinimized)
    {
      ShowNotification(
        NULL, MainInstructions(LoadStr(BALLOON_OPERATION_COMPLETE)),
        qtInformation);
    }

    if (!FFocusPath.IsEmpty())
    {
      UnicodeString FocusPath = FFocusPath;
      FFocusPath = L"";
      DoFocusFile(FocusPath);
    }

    UpdateControls();
  }

  if (FClosePending)
  {
    FClosePending = false;
    Close();
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::FileFound(TTerminal * /*Terminal*/,
  const UnicodeString FileName, const TRemoteFile * AFile, bool & Cancel)
{
  TListItem * Item;
  int Count = FileView->Items->Count;
  if ((GetColProperties()->SortColumn == NoSort) || (Count == 0))
  {
    Item = FileView->Items->Add();
  }
  else
  {
    TRemoteFile * FirstFile = static_cast<TRemoteFile *>(FileView->Items->Item[0]->Data);
    TRemoteFile * LastFile = static_cast<TRemoteFile *>(FileView->Items->Item[Count - 1]->Data);
    if (FilesCompare(AFile, FirstFile) < 0)
    {
      Item = FileView->Items->Insert(0);
    }
    else if (FilesCompare(LastFile, AFile) < 0)
    {
      Item = FileView->Items->Add();
    }
    else
    {
      int Start = 0;
      int End = Count;
      while (Start < End - 1)
      {
        int Index = (Start + End) / 2;
        DebugAssert((Index >= 0) && (Index < End));
        TRemoteFile * FileAtIndex = static_cast<TRemoteFile *>(FileView->Items->Item[Index]->Data);
        int Compare = FilesCompare(AFile, FileAtIndex);
        if (Compare <= 0)
        {
          End = Index;
        }
        else
        {
          Start = Index;
        }
      }
      Item = FileView->Items->Insert(Start + 1);
    }
  }

  TRemoteFile * File = AFile->Duplicate(true);
  Item->Data = File;

  Item->ImageIndex = File->IconIndex;
  UnicodeString Caption = File->FileName;
  if (File->IsDirectory)
  {
    Caption = UnixIncludeTrailingBackslash(Caption);
  }
  Item->Caption = Caption;

  UnicodeString Directory = UnixExtractFilePath(File->FullFileName);
  if (AnsiSameText(FDirectory, Directory.SubString(1, FDirectory.Length())))
  {
    Directory = L"." + Directory.SubString(FDirectory.Length() + 1, Directory.Length() - FDirectory.Length());
  }
  else
  {
    DebugFail();
  }
  Item->SubItems->Add(Directory);

  if (File->IsDirectory)
  {
    Item->SubItems->Add(L"");
  }
  else
  {
    Item->SubItems->Add(
      FormatPanelBytes(File->Size, WinConfiguration->FormatSizeBytes));
  }
  Item->SubItems->Add(UserModificationStr(File->Modification, File->ModificationFmt));

  UpdateControls();
  Cancel = (FState == ffAborting);
  Application->ProcessMessages();
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::FindingFile(TTerminal * /*Terminal*/,
  const UnicodeString Directory, bool & Cancel)
{
  if (!Directory.IsEmpty() && (FFindingInDirectory != Directory))
  {
    FFindingInDirectory = Directory;
    UpdateControls();
  }

  Cancel = (FState == ffAborting);
  Application->ProcessMessages();
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::StartStopButtonClick(TObject * /*Sender*/)
{
  if (IsFinding())
  {
    Stop();
  }
  else
  {
    Clear();
    if (ActiveControl->Parent == FilterGroup)
    {
      FileView->SetFocus();
    }
    Start();
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::StopButtonClick(TObject * /*Sender*/)
{
  Stop();
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::Stop()
{
  FState = ffAborting;
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::FormShow(TObject * /*Sender*/)
{
  InstallPathWordBreakProc(MaskEdit);
  InstallPathWordBreakProc(RemoteDirectoryEdit);

  // have to set history after value, to prevent autocompletition
  MaskEdit->Items = WinConfiguration->History[L"Mask"];
  RemoteDirectoryEdit->Items = CustomWinConfiguration->History[L"RemoteDirectory"];

  UpdateFormPosition(this, poOwnerFormCenter);
  RestoreFormSize(CustomWinConfiguration->FindFile.WindowParams, this);
  UnicodeString S = CustomWinConfiguration->FindFile.ListParams;
  UnicodeString ParamsStr = CutToChar(S, L'/', true);
  FileView->ColProperties->ParamsStr = ParamsStr;
  UnicodeString V = CutToChar(S, L'/', true);
  if (StrToIntDef(V, 0) == 0)
  {
    // Old versions had non-sense (while unused) default to sorting by "Changed", ignore it.
    GetColProperties()->SortColumn = NoSort;
  }

  DebugAssert(FWindowParams.IsEmpty());
  if (FWindowParams.IsEmpty())
  {
    FWindowParams = StoreFormSize(this);
  }

  UpdateControls();
}
//---------------------------------------------------------------------------
bool __fastcall TFileFindDialog::StopIfFinding()
{
  bool Result = IsFinding();
  if (Result)
  {
    Stop();
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::FormCloseQuery(TObject * /*Sender*/,
  bool & CanClose)
{
  if (StopIfFinding())
  {
    FClosePending = true;
    CanClose = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::Dispatch(void * Message)
{
  TMessage * M = reinterpret_cast<TMessage*>(Message);
  if (M->Msg == CM_DIALOGKEY)
  {
    CMDialogKey(*((TWMKeyDown *)Message));
  }
  else
  {
    TForm::Dispatch(Message);
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::FormAfterMonitorDpiChanged(TObject *, int OldDPI, int NewDPI)
{
  DebugUsedParam2(OldDPI, NewDPI);
  UpdateImages();
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::CMDialogKey(TWMKeyDown & Message)
{
  // Handling VK_ESCAPE in FormKeyDown causes a beep when any "edit" has focus.
  // Moreover FormKeyDown is called when the the "esc" is pressed while drop down list is unrolled.
  if (Message.CharCode == VK_ESCAPE)
  {
    if (!StopIfFinding())
    {
      Close();
    }
    Message.Result = 1;
    return;
  }
  TForm::Dispatch(&Message);
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::FormKeyDown(TObject * /*Sender*/, WORD & Key,
  TShiftState Shift)
{
  if ((Key == L'C') && Shift.Contains(ssCtrl) &&
      (dynamic_cast<TCustomCombo *>(ActiveControl) == NULL))
  {
    CopyToClipboard();
    Key = 0;
  }
  else if ((Key == VK_F10) && Shift.Empty())
  {
    Key = 0;
    StopIfFinding();
    Close();
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::MaskEditExit(TObject * /*Sender*/)
{
  ValidateMaskEdit(MaskEdit);
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::DoFocusFile(const UnicodeString & Path)
{
  FOnFocusFile(FTerminal, Path);
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::FocusFile()
{
  UnicodeString Path = static_cast<TRemoteFile *>(FileView->ItemFocused->Data)->FullFileName;
  // To make focussing directories work,
  // otherwise it would try to focus "empty-named file" in the Path
  Path = UnixExcludeTrailingBackslash(Path);

  if (StopIfFinding())
  {
    FFocusPath = Path;
  }
  else
  {
    DoFocusFile(Path);
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::FileViewDblClick(TObject * /*Sender*/)
{
  if (FileView->ItemFocused != NULL)
  {
    FocusFile();
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::FocusActionExecute(TObject * /*Sender*/)
{
  FocusFile();
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::FileViewSelectItem(TObject * /*Sender*/,
  TListItem * /*Item*/, bool /*Selected*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::MaskButtonClick(TObject * /*Sender*/)
{
  TFileMasks Masks = MaskEdit->Text;
  if (DoEditMaskDialog(Masks))
  {
    MaskEdit->Text = Masks.Masks;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::CopyToClipboard()
{
  TInstantOperationVisualizer Visualizer;
  std::unique_ptr<TStrings> Strings(new TStringList());
  for (int Index = 0; Index < FileView->Items->Count; Index++)
  {
    TListItem * Item = FileView->Items->Item[Index];
    TRemoteFile * File = static_cast<TRemoteFile *>(Item->Data);
    Strings->Add(File->FullFileName);
  }
  ::CopyToClipboard(Strings.get());
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::CopyActionExecute(TObject * /*Sender*/)
{
  CopyToClipboard();
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::FormClose(TObject * /*Sender*/, TCloseAction & Action)
{
  StopIfFinding();
  Action = caFree;
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::FileViewContextPopup(TObject * Sender, TPoint & MousePos, bool & Handled)
{
  // to update source popup menu before TBX menu is created
  UpdateControls();
  MenuPopup(Sender, MousePos, Handled);
}
//---------------------------------------------------------------------------
TListItem * __fastcall TFileFindDialog::FileOperationFinished(const UnicodeString & FileName)
{
  TFileItemMap::iterator I = FFileItemMap.find(FileName);

  TListItem * Result = NULL;
  if (DebugAlwaysTrue(I != FFileItemMap.end()))
  {
    Result = I->second;
    FileView->MakeProgressVisible(Result);
    FFileItemMap.erase(I);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::FileDeleteFinished(
  TOperationSide, const UnicodeString & FileName, bool Success, bool NotCancelled)
{
  DebugUsedParam(NotCancelled);
  if (FileName.IsEmpty())
  {
    DebugAssert(Success && NotCancelled);
    FileView->SelectAll(smNone);
  }
  else
  {
    TListItem * Item = FileOperationFinished(FileName);
    if (DebugAlwaysTrue(Item != NULL) && Success)
    {
      ClearItem(Item);
      Item->Delete();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::FileDownloadFinished(
  TOperationSide, const UnicodeString & FileName, bool Success, bool NotCancelled)
{
  if (FileName.IsEmpty())
  {
    DebugAssert(Success && NotCancelled);
    // Moved to queue, see call in TCustomScpExplorerForm::CopyParamDialog
    FileView->SelectAll(smNone);
  }
  else
  {
    TListItem * Item = FileOperationFinished(FileName);
    if (DebugAlwaysTrue(Item != NULL) && Success && NotCancelled)
    {
      Item->Selected = false;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::FileListOperation(
  TFileListOperationEvent Operation, TFileOperationFinishedEvent OnFileOperationFinished)
{
  std::unique_ptr<TStrings> FileList(new TStringList());

  DebugAssert(FFileItemMap.empty());
  TListItem * Item = FileView->Selected;
  while (Item != NULL)
  {
    TRemoteFile * File = static_cast<TRemoteFile *>(Item->Data);
    FileList->AddObject(File->FullFileName, File);
    FFileItemMap.insert(std::make_pair(File->FullFileName, Item));
    Item = FileView->GetNextItem(Item, sdAll, TItemStates() << isSelected);
  }

  try
  {
    Operation(FTerminal, FileList.get(), OnFileOperationFinished);
  }
  __finally
  {
    // can be non-empty only when not all files were processed
    FFileItemMap.clear();
  }

  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::DeleteActionExecute(TObject * /*Sender*/)
{
  FileListOperation(FOnDeleteFiles, FileDeleteFinished);
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::SelectAllActionExecute(TObject * /*Sender*/)
{
  FileView->SelectAll(smAll);
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::DownloadActionExecute(TObject * /*Sender*/)
{
  FileListOperation(FOnDownloadFiles, FileDownloadFinished);
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::EditActionExecute(TObject * /*Sender*/)
{
  FileListOperation(FOnEditFiles, FileDownloadFinished);
}
//---------------------------------------------------------------------------
TIEListViewColProperties * TFileFindDialog::GetColProperties()
{
  return dynamic_cast<TIEListViewColProperties *>(FileView->ColProperties);
}
//---------------------------------------------------------------------------
int TFileFindDialog::FilesCompare(const TRemoteFile * File1, const TRemoteFile * File2)
{
  int Result = 0;
  switch (GetColProperties()->SortColumn)
  {
    case 0: // name
      Result = CompareText(File1->FileName, File2->FileName);
      break;

    case NoSort: // directories
      Result = 0; // default sort
      break;

    case 2: // size
      Result = CompareNumber(File1->Size, File2->Size);
      break;

    case 3: // changed
      Result = CompareFileTime(File1->Modification, File2->Modification);
      break;
  }

  if (Result == 0)
  {
    Result = CompareText(UnixExtractFilePath(File1->FullFileName), UnixExtractFilePath(File2->FullFileName));

    if (Result == 0)
    {
      Result = CompareText(File1->FileName, File2->FileName);
    }
  }

  if (!GetColProperties()->SortAscending)
  {
    Result = -Result;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::FileViewCompare(TObject *, TListItem * Item1, TListItem * Item2, int DebugUsedArg(Data), int & Compare)
{
  TRemoteFile * File1 = static_cast<TRemoteFile *>(Item1->Data);
  TRemoteFile * File2 = static_cast<TRemoteFile *>(Item2->Data);

  Compare = FilesCompare(File1, File2);
}
//---------------------------------------------------------------------------
