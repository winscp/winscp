//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <WinInterface.h>
#include <ScpMain.h>
#include <TextsWin.h>
#include <VCLCommon.h>
#include <CustomWinConfiguration.h>

#include "Copy.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "MoreButton"
#pragma link "Rights"
#pragma link "CopyParams"
#pragma link "HistoryComboBox"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
bool __fastcall DoCopyDialog(bool ToRemote,
  bool Move, bool DragDrop, TStrings * FileList,
  bool AllowTransferMode, AnsiString & TargetDirectory,
  TCopyParamType * Params, int & Options, bool AllowDirectory)
{
  bool Result;
  TCopyDialog *CopyDialog = new TCopyDialog(Application);
  try
  {
    if (!AllowTransferMode)
    {
      // If local and remote EOL types are the same, there is no need
      // for ASCII (or Automatic) mode
      CopyDialog->AllowTransferMode = false; // Default is true
      Params->TransferMode = tmBinary;
    }
    CopyDialog->ToRemote = ToRemote;
    CopyDialog->DragDrop = DragDrop && !ToRemote;
    if (!CopyDialog->DragDrop) CopyDialog->Directory = TargetDirectory;
    CopyDialog->FileList = FileList;
    CopyDialog->Params = *Params;
    CopyDialog->Move = Move;
    CopyDialog->AllowDirectory = AllowDirectory;
    CopyDialog->Options = Options;
    Result = CopyDialog->Execute();
    if (Result)
    {
      if (!CopyDialog->DragDrop) TargetDirectory = CopyDialog->Directory;
      Params->Assign(CopyDialog->Params);
      Options = CopyDialog->Options;
    }
  }
  __finally
  {
    delete CopyDialog;
  }
  return Result;
}
//---------------------------------------------------------------------------
__fastcall TCopyDialog::TCopyDialog(TComponent* Owner)
        : TForm(Owner)
{
  // on start set different value than we want to allow property-setter to proceed
  FToRemote = false;
  FMove = true;

  ToRemote = true;
  Move = false;
  AllowTransferMode = true;
  AllowDirectory = true;
  FOptions = 0;

  UseSystemSettings(this);
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::SetToRemote(bool value)
{
  if (FToRemote != value)
  {
    AnsiString ADirectory = DirectoryEdit->Text;
    FToRemote = value;
    DirectoryEdit->Text = ADirectory;

    RemoteDirectoryEdit->Visible = false;
    LocalDirectoryEdit->Visible = false;
    DirectoryEdit->Visible = !DragDrop;
    DirectoryEdit->Enabled = AllowDirectory;
    DirectoryLabel->FocusControl = DirectoryEdit;
    UpdateControls();
    CopyParamsFrame->Direction = !ToRemote ? pdToLocal : pdToRemote;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::SetAllowDirectory(bool value)
{
  if (AllowDirectory != value)
  {
    FAllowDirectory = value;
    DirectoryEdit->Enabled = AllowDirectory;
  }
}
//---------------------------------------------------------------------------
int __fastcall TCopyDialog::GetOptions()
{
  return
    (FOptions & ~(coQueue | coQueueNoConfirmation)) |
    (QueueCheck->Checked ? coQueue : 0) |
    (QueueNoConfirmationCheck->Checked ? coQueueNoConfirmation : 0);
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::SetOptions(int value)
{
  FOptions = value;
  QueueCheck->Checked = ((value & coQueue) != 0);
  QueueNoConfirmationCheck->Checked = ((value & coQueueNoConfirmation) != 0);
}
//---------------------------------------------------------------------------
THistoryComboBox * __fastcall TCopyDialog::GetDirectoryEdit()
{
  return ToRemote ? RemoteDirectoryEdit : LocalDirectoryEdit;
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::SetFileMask(const AnsiString value)
{
  if (!DragDrop)
  {
    DirectoryEdit->Text = Directory + value;
  }
  else
  {
    FFileMask = value;
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall TCopyDialog::GetFileMask()
{
  if (!DragDrop)
  {
    return ToRemote ? UnixExtractFileName(DirectoryEdit->Text) :
      ExtractFileName(DirectoryEdit->Text);
  }
  else
  {
    return FFileMask;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::SetParams(TCopyParamType value)
{
  CopyParamsFrame->Params = value;
  SetFileMask(value.FileMask);
}
//---------------------------------------------------------------------------
TCopyParamType __fastcall TCopyDialog::GetParams()
{
  TCopyParamType Params = CopyParamsFrame->Params;
  Params.FileMask = GetFileMask();
  return Params;
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::SetDirectory(AnsiString value)
{
  value = ToRemote ? UnixIncludeTrailingBackslash(value) :
    IncludeTrailingBackslash(value);
  DirectoryEdit->Text = value + GetFileMask();
}
//---------------------------------------------------------------------------
AnsiString __fastcall TCopyDialog::GetDirectory()
{
  assert(DirectoryEdit && !DragDrop);

  AnsiString Result = DirectoryEdit->Text;
  if (ToRemote)
  {
    Result = UnixIncludeTrailingBackslash(UnixExtractFilePath(Result));
  }
  else
  {
    Result = IncludeTrailingBackslash(ExtractFilePath(Result));
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::SetFileList(TStrings * value)
{
  if (FFileList != value)
  {
    FFileList = value;
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::UpdateControls()
{
  if (FileList && FileList->Count)
  {
    AnsiString TransferStr = LoadStr(!Move ? COPY_COPY : COPY_MOVE);
    AnsiString DirectionStr =
      LoadStr((DragDrop ? COPY_TODROP : (!ToRemote ? COPY_TOLOCAL : COPY_TOREMOTE)));

    if (FileList->Count == 1)
    {
      AnsiString FileName;
      if (!ToRemote) FileName = UnixExtractFileName(FFileList->Strings[0]);
        else FileName = ExtractFileName(FFileList->Strings[0]);
      DirectoryLabel->Caption = FMTLOAD(COPY_FILE,
        (TransferStr, FileName, DirectionStr));
    }
    else
    {
      DirectoryLabel->Caption = FMTLOAD(COPY_FILES,
        (TransferStr, FFileList->Count, DirectionStr));
    }
  }

  if (!Move)
  {
    Caption = LoadStr(COPY_COPY_CAPTION);
    CopyButton->Caption = LoadStr(COPY_COPY_BUTTON);
  }
  else
  {
    Caption = LoadStr(COPY_MOVE_CAPTION);
    CopyButton->Caption = LoadStr(COPY_MOVE_BUTTON);
  }

  LocalDirectoryBrowseButton->Visible = !ToRemote && !DragDrop && AllowDirectory;

  EnableControl(QueueCheck, !DragDrop && ((Options & coQueueDisable) == 0));
  EnableControl(QueueNoConfirmationCheck, !DragDrop && QueueCheck->Checked);
  QueueNoConfirmationCheck->Visible = MoreButton->Expanded;
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::SetDragDrop(Boolean value)
{
  if (DragDrop != value)
  {
    FDragDrop = value;
    if (value)
    {
      ToRemote = false;
      DirectoryEdit->Text = "";
    }
    DirectoryEdit->Visible = !value || ToRemote;
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::SetMove(bool value)
{
  if (Move != value)
  {
    FMove = value;
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::FormShow(TObject * /*Sender*/)
{
  assert(FileList && (FileList->Count > 0) && (!ToRemote || !DragDrop));
  if (!DragDrop && AllowDirectory) DirectoryEdit->SetFocus();
    else
  if (MoreButton->Expanded) MorePanel->SetFocus();
    else CopyButton->SetFocus();
  UpdateControls();
}
//---------------------------------------------------------------------------
bool __fastcall TCopyDialog::Execute()
{
  DirectoryEdit->Items = CustomWinConfiguration->History[
    ToRemote ? "RemoteTarget" : "LocalTarget"];
  SaveSettingsCheck->Checked = false;
  MoreButton->Expanded = GUIConfiguration->CopyParamDialogExpanded;
  CopyParamsFrame->BeforeExecute();
  bool Result = (ShowModal() == mrOk);
  if (Result)
  {
    CopyParamsFrame->AfterExecute();
    Configuration->BeginUpdate();
    try
    {
      GUIConfiguration->CopyParamDialogExpanded = MoreButton->Expanded;
      if (SaveSettingsCheck->Checked)
      {
        Configuration->CopyParam = Params;
      }
      DirectoryEdit->SaveToHistory();
      CustomWinConfiguration->History[ToRemote ?
        "RemoteTarget" : "LocalTarget"] = DirectoryEdit->Items;
    }
    __finally
    {
      Configuration->EndUpdate();
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::FormCloseQuery(TObject * /*Sender*/,
      bool &CanClose)
{
  if (ModalResult != mrCancel)
  {
    if (!ToRemote && !DragDrop)
    {
      AnsiString Dir = Directory;
      AnsiString Drive = ExtractFileDrive(Dir);
      if (Drive.IsEmpty() || (Drive.Length() != 2) || (Drive[2] != ':'))
      {
        SimpleErrorDialog(LoadStr(ABSOLUTE_PATH_REQUIRED));
        CanClose = false;
      }
      else if (!DirectoryExists(Dir))
      {
        if (MessageDialog(FMTLOAD(CREATE_LOCAL_DIRECTORY, (Dir)),
              qtConfirmation, qaOK | qaCancel, 0) != qaCancel)
        {
          if (!ForceDirectories(Dir))
          {
            SimpleErrorDialog(FMTLOAD(CREATE_LOCAL_DIR_ERROR, (Dir)));
            CanClose = false;
          }
        }
        else
        {
          CanClose = False;
        }
      }

      if (!CanClose)
      {
        DirectoryEdit->SelectAll();
        DirectoryEdit->SetFocus();
      }
    };

    if (CanClose && (Params.TransferMode == tmAutomatic))
    {
      TFileMasks Masks = CopyParamsFrame->AsciiFileMask;
      int Start, Length;
      if (!Masks.IsValid(Start, Length))
      {
        MoreButton->Expanded = true;
        CanClose = false;
        SimpleErrorDialog(FMTLOAD(MASK_ERROR, (Masks.Masks.SubString(Start+1, Length))));
        // After closing dialog whole text is selected, we want to select only invalid mask
        CopyParamsFrame->SelectMask(Start, Length);
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::SetAllowTransferMode(Boolean value)
{
  CopyParamsFrame->AllowTransferMode = value;
}
//---------------------------------------------------------------------------
bool __fastcall TCopyDialog::GetAllowTransferMode()
{
  return CopyParamsFrame->AllowTransferMode;
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::LocalDirectoryBrowseButtonClick(
      TObject * /*Sender*/)
{
  assert(!ToRemote);
  AnsiString Directory = LocalDirectoryEdit->Text;
  if (SelectDirectory(Directory, LoadStr(SELECT_LOCAL_DIRECTORY), true))
  {
    LocalDirectoryEdit->Text = Directory;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::DirectoryEditKeyDown(TObject * Sender,
  WORD & Key, TShiftState Shift)
{
  PathComboBoxKeyDown(dynamic_cast<TCustomComboBox*>(Sender), Key, Shift,
    (Sender == RemoteDirectoryEdit));
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::ControlChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------


