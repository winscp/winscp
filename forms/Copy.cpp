//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <UnixDirView.h>

#include <Common.h>
#include <WinInterface.h>
#include <ScpMain.h>
#include <TextsWin.h>
#include <VCLCommon.h>

#include "Copy.h"
#include "WinConfiguration.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "ComboEdit"
#pragma link "MoreButton"
#pragma link "Rights"
#pragma link "CopyParams"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
bool __fastcall DoCopyDialog(TTransferDirection Direction,
  TTransferType Type, bool DragDrop, TStrings * FileList,
  bool AllowTransferMode, AnsiString & TargetDirectory,
  TCopyParamType * Params)
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
    CopyDialog->Direction = Direction;
    CopyDialog->DragDrop = DragDrop && (Direction == tdToLocal);
    if (!CopyDialog->DragDrop) CopyDialog->Directory = TargetDirectory;
    CopyDialog->FileList = FileList;
    CopyDialog->Params = *Params;
    CopyDialog->TransferType = Type;
    Result = CopyDialog->Execute();
    if (Result)
    {
      if (!CopyDialog->DragDrop) TargetDirectory = CopyDialog->Directory;
      Params->Assign(CopyDialog->Params);
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
  FDirection = tdToLocal;
  FTransferType = ttMove;

  Direction = tdToRemote;
  TransferType = ttCopy;
  AllowTransferMode = True;

  UseSystemFont(this);
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::SetDirection(TTransferDirection value)
{
  if (FDirection != value)
  {
    AnsiString ADirectory = DirectoryEdit->Text;
    FDirection = value;
    DirectoryEdit->Text = ADirectory;

    RemoteDirectoryEdit->Visible = False;
    LocalDirectoryEdit->Visible = False;
    DirectoryEdit->Visible = !DragDrop;
    DirectoryLabel->FocusControl = DirectoryEdit;
    UpdateControls();
    CopyParamsFrame->Direction = Direction == tdToLocal ? pdToLocal : pdToRemote;
  }
}
//---------------------------------------------------------------------------
TCustomEdit * __fastcall TCopyDialog::GetDirectoryEdit()
{
  assert((Direction == tdToRemote) || (Direction == tdToLocal));
  return Direction == tdToRemote ? (TCustomEdit *)RemoteDirectoryEdit :
    (TCustomEdit *)LocalDirectoryEdit;
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::SetParams(TCopyParamType value)
{
  CopyParamsFrame->Params = value;
}
//---------------------------------------------------------------------------
TCopyParamType __fastcall TCopyDialog::GetParams()
{
  TCopyParamType Params;
  Params = CopyParamsFrame->Params;
  return Params;
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::SetDirectory(AnsiString value)
{
  DirectoryEdit->Text = value;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TCopyDialog::GetDirectory()
{
  assert(((Direction == tdToRemote) || (Direction == tdToLocal)) &&
    DirectoryEdit && !DragDrop);

  AnsiString Result = DirectoryEdit->Text;
  if (Direction == tdToRemote) Result = UnixIncludeTrailingBackslash(Result);
    else Result = IncludeTrailingBackslash(Result);
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
    assert((Direction == tdToRemote) || (Direction == tdToLocal));

    AnsiString TransferStr =
      LoadStr(FTransferType == ttCopy ? COPY_COPY : COPY_MOVE);
    AnsiString DirectionStr =
      LoadStr((DragDrop ? COPY_TODROP :
         (FDirection == tdToLocal ? COPY_TOLOCAL : COPY_TOREMOTE)));

    if (FileList->Count == 1)
    {
      AnsiString FileName;
      if (FDirection == tdToLocal) FileName = UnixExtractFileName(FFileList->Strings[0]);
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

  if (TransferType == ttCopy)
  {
    Caption = LoadStr(COPY_COPY_CAPTION);
    CopyButton->Caption = LoadStr(COPY_COPY_BUTTON);
  }
  else
  {
    Caption = LoadStr(COPY_MOVE_CAPTION);
    CopyButton->Caption = LoadStr(COPY_MOVE_BUTTON);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::SetDragDrop(Boolean value)
{
  if (DragDrop != value)
  {
    FDragDrop = value;
    if (value)
    {
      Direction = tdToLocal;
      DirectoryEdit->Text = "";
    }
    DirectoryEdit->Visible = !value || (Direction == tdToRemote);
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::SetTransferType(TTransferType value)
{
  if (TransferType != value)
  {
    FTransferType = value;
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::FormShow(TObject * /*Sender*/)
{
  assert(FileList && (FileList->Count > 0) &&
    ((Direction == tdToLocal) || !DragDrop));
  if (!DragDrop) DirectoryEdit->SetFocus();
    else
  if (MoreButton->Expanded) MorePanel->SetFocus();
    else CopyButton->SetFocus();
}
//---------------------------------------------------------------------------
bool __fastcall TCopyDialog::Execute()
{
  SaveSettingsCheck->Checked = false;
  MoreButton->Expanded =
    WinConfiguration->CopyParamDialogExpanded && WinConfiguration->ExpertMode;
  MoreButton->Visible = WinConfiguration->ExpertMode;
  CopyParamsFrame->BeforeExecute();
  bool Result = (ShowModal() == mrOk);
  if (Result)
  {
    CopyParamsFrame->AfterExecute();
    Configuration->BeginUpdate();
    try
    {
      WinConfiguration->CopyParamDialogExpanded = MoreButton->Expanded;
      if (SaveSettingsCheck->Checked)
      {
        Configuration->CopyParam = Params;
      }
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
    if ((Direction == tdToLocal) && !DragDrop && !DirectoryExists(Directory))
    {
      if (MessageDialog(FMTLOAD(CREATE_LOCAL_DIRECTORY, (Directory)),
            qtConfirmation, qaOK | qaCancel, 0) != qaCancel)
      {
        if (!ForceDirectories(Directory))
        {
          SimpleErrorDialog(FMTLOAD(CREATE_LOCAL_DIR_ERROR, (Directory)));
          CanClose = false;
        }
      }
      else
      {
        CanClose = False;
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
