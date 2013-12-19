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
#include "FileFind.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "HistoryComboBox"
#pragma link "IEListView"
#pragma link "NortonLikeListView"
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------------
bool __fastcall DoFileFindDialog(UnicodeString Directory,
  TFindEvent OnFind, UnicodeString & Path)
{
  bool Result;
  TFileFindDialog * Dialog = new TFileFindDialog(Application, OnFind);
  try
  {
    Result = Dialog->Execute(Directory, Path);
  }
  __finally
  {
    delete Dialog;
  }

  return Result;
}
//---------------------------------------------------------------------------
__fastcall TFileFindDialog::TFileFindDialog(TComponent * Owner, TFindEvent OnFind)
  : TForm(Owner)
{
  UseSystemSettings(this);
  FOnFind = OnFind;
  FState = ffInit;
  FMinimizedByMe = false;

  FixComboBoxResizeBug(MaskEdit);
  FixComboBoxResizeBug(RemoteDirectoryEdit);
  HintLabel(MaskHintText,
    FORMAT(L"%s\n \n%s\n \n%s\n \n%s\n \n%s\n \n%s", (LoadStr(MASK_HINT2),
      LoadStr(FILE_MASK_EX_HINT), LoadStr(COMBINING_MASKS_HINT),
      LoadStr(PATH_MASK_HINT2), LoadStr(DIRECTORY_MASK_HINT),
      LoadStr(MASK_HELP))));

  FSystemImageList = SharedSystemImageList(false);
  FileView->SmallImages = FSystemImageList;

  UseDesktopFont(FileView);
  UseDesktopFont(StatusBar);

  SetGlobalMinimizeHandler(this, GlobalMinimize);
}
//---------------------------------------------------------------------------
__fastcall TFileFindDialog::~TFileFindDialog()
{
  ClearGlobalMinimizeHandler(GlobalMinimize);

  Clear();
  delete FSystemImageList;
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
  Caption = LoadStr(Finding ? FIND_FILE_FINDING : FIND_FILE_TITLE);
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
  CancelButton->Visible = !Finding;
  EnableControl(FilterGroup, !Finding);
  MinimizeButton->Visible = Finding;
  EnableControl(FocusButton, (FileView->ItemFocused != NULL));
  switch (FState)
  {
    case ffInit:
      StatusBar->SimpleText = L"";

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
      assert(false);
      break;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::ControlChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
bool __fastcall TFileFindDialog::Execute(UnicodeString Directory, UnicodeString & Path)
{
  MaskEdit->Text = WinConfiguration->SelectMask;
  RemoteDirectoryEdit->Text = UnixExcludeTrailingBackslash(Directory);

  // have to set history after value, to prevent autocompletition
  MaskEdit->Items = WinConfiguration->History[L"Mask"];
  RemoteDirectoryEdit->Items = CustomWinConfiguration->History[L"RemoteDirectory"];

  bool Result = (ShowModal() == FocusButton->ModalResult);
  if (Result)
  {
    Path = static_cast<TRemoteFile *>(FileView->ItemFocused->Data)->FullFileName;
  }

  TFindFileConfiguration FormConfiguration = CustomWinConfiguration->FindFile;
  FormConfiguration.ListParams = FileView->ColProperties->ParamsStr;
  FormConfiguration.WindowParams = StoreFormSize(this);
  CustomWinConfiguration->FindFile = FormConfiguration;

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::Clear()
{
  for (int Index = 0; Index < FileView->Items->Count; Index++)
  {
    TListItem * Item = FileView->Items->Item[Index];
    TRemoteFile * File = static_cast<TRemoteFile *>(Item->Data);
    Item->Data = NULL;
    delete File;
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

  assert(FState != ffFinding);

  FState = ffFinding;
  try
  {
    UpdateControls();
    Repaint();

    TOperationVisualizer Visualizer;

    assert(FOnFind != NULL);
    FDirectory = UnixExcludeTrailingBackslash(RemoteDirectoryEdit->Text);
    FOnFind(FDirectory, MaskEdit->Text, FileFound, FindingFile);
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
    if (IsApplicationMinimized() && FMinimizedByMe)
    {
      ShowNotification(NULL, LoadStr(BALLOON_OPERATION_COMPLETE), qtInformation);
      FMinimizedByMe = false;
    }
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::FileFound(TTerminal * /*Terminal*/,
  const UnicodeString FileName, const TRemoteFile * AFile, bool & Cancel)
{
  TListItem * Item = FileView->Items->Add();
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
    Directory[1] = L'.';
    Directory.Delete(2, FDirectory.Length() - 1);
  }
  else
  {
    assert(false);
  }
  Item->SubItems->Add(Directory);

  if (File->IsDirectory)
  {
    Item->SubItems->Add(L"");
  }
  else
  {
    Item->SubItems->Add(
      FormatBytes(File->Size,
        WinConfiguration->FormatSizeBytes, WinConfiguration->FormatSizeBytes));
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
void __fastcall TFileFindDialog::MinimizeButtonClick(TObject * Sender)
{
  CallGlobalMinimizeHandler(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::GlobalMinimize(TObject * /*Sender*/)
{
  Application->Minimize();
  FMinimizedByMe = true;
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::FormShow(TObject * /*Sender*/)
{
  InstallPathWordBreakProc(MaskEdit);
  InstallPathWordBreakProc(RemoteDirectoryEdit);

  UpdateFormPosition(this, poOwnerFormCenter);
  RestoreFormSize(CustomWinConfiguration->FindFile.WindowParams, this);
  FileView->ColProperties->ParamsStr = CustomWinConfiguration->FindFile.ListParams;
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
  bool & /*CanClose*/)
{
  StopIfFinding();
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::FormKeyDown(TObject * /*Sender*/, WORD & Key,
  TShiftState /*Shift*/)
{
  if ((Key == VK_ESCAPE) && StopIfFinding())
  {
    Key = 0;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::MaskEditExit(TObject * /*Sender*/)
{
  ValidateMaskEdit(MaskEdit);
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::FileViewDblClick(TObject * /*Sender*/)
{
  if (FileView->ItemFocused != NULL)
  {
    StopIfFinding();
    ModalResult = FocusButton->ModalResult;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileFindDialog::FocusButtonClick(TObject * /*Sender*/)
{
  StopIfFinding();
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
