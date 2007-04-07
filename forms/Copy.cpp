//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <WinInterface.h>
#include <CoreMain.h>
#include <TextsWin.h>
#include <VCLCommon.h>
#include <CustomWinConfiguration.h>
#include <Tools.h>

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
  bool Move, TStrings * FileList, AnsiString & TargetDirectory,
  TGUICopyParamType * Params, int Options, int CopyParamAttrs, int * OutputOptions)
{
  bool Result;
  TCopyDialog *CopyDialog = new TCopyDialog(Application);
  try
  {
    if (FLAGSET(CopyParamAttrs, cpaNoTransferMode))
    {
      // If local and remote EOL types are the same, there is no need
      // for ASCII (or Automatic) mode
      Params->TransferMode = tmBinary;
    }
    CopyDialog->ToRemote = ToRemote;
    CopyDialog->Options = Options;
    CopyDialog->CopyParamAttrs = CopyParamAttrs;
    if (OutputOptions != NULL)
    {
      CopyDialog->OutputOptions = *OutputOptions;
    }
    CopyDialog->Directory = TargetDirectory;
    CopyDialog->FileList = FileList;
    CopyDialog->Params = *Params;
    CopyDialog->Move = Move;
    Result = CopyDialog->Execute();
    if (Result)
    {
      TargetDirectory = CopyDialog->Directory;
      *Params = CopyDialog->Params;
      if (OutputOptions != NULL)
      {
        *OutputOptions = CopyDialog->OutputOptions;
      }
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
  FOptions = 0;
  FOutputOptions = 0;
  FPresetsMenu = new TPopupMenu(this);

  UseSystemSettings(this);
}
//---------------------------------------------------------------------------
__fastcall TCopyDialog::~TCopyDialog()
{
  delete FPresetsMenu;
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::AdjustTransferControls()
{
  if (FileList && FileList->Count)
  {
    if (!ToRemote && !Move && FLAGSET(FOutputOptions, cooRemoteTransfer))
    {
      AnsiString Label;
      if (FileList->Count == 1)
      {
        AnsiString FileName;
        if (!ToRemote) FileName = UnixExtractFileName(FFileList->Strings[0]);
          else FileName = ExtractFileName(FFileList->Strings[0]);
        Label = FMTLOAD(REMOTE_COPY_FILE, (FileName));
      }
      else
      {
        Label = FMTLOAD(REMOTE_COPY_FILES, (FFileList->Count));
      }

      // hack to remove trailing colon used for "duplicate" prompt
      if (!Label.IsEmpty() &&
          (Label.ByteType(Label.Length()) == mbSingleByte) &&
          (Label[Label.Length()] == ':'))
      {
        Label.SetLength(Label.Length() - 1);
      }

      DirectoryLabel->Caption = Label;
    }
    else
    {
      AnsiString TransferStr = LoadStr(!Move ? COPY_COPY : COPY_MOVE);
      // currently the copy dialog is shown when downloading to temp folder
      // only for drag&drop downloads, for we dare to display d&d specific prompt
      AnsiString DirectionStr =
        LoadStr(((Options & coTemp) != 0) ? COPY_TODROP :
          (RemotePaths() ? COPY_TOREMOTE : COPY_TOLOCAL));

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
  }

  if (!Move)
  {
    if (!ToRemote && FLAGSET(FOutputOptions, cooRemoteTransfer))
    {
      Caption = LoadStr(REMOTE_COPY_TITLE);
    }
    else
    {
      Caption = LoadStr(COPY_COPY_CAPTION);
    }
    CopyButton->Caption = LoadStr(COPY_COPY_BUTTON);
  }
  else
  {
    Caption = LoadStr(COPY_MOVE_CAPTION);
    CopyButton->Caption = LoadStr(COPY_MOVE_BUTTON);
  }

  bool RemoteTransfer = FLAGSET(FOutputOptions, cooRemoteTransfer);
  assert(FLAGSET(Options, coAllowRemoteTransfer) || !RemoteTransfer);

  EnableControl(CopyParamsFrame, !RemoteTransfer);
  EnableControl(NewerOnlyCheck, FLAGCLEAR(Options, coDisableNewerOnly) && !RemoteTransfer);
  EnableControl(PresetsButton, !RemoteTransfer);
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::AdjustControls()
{
  NeverShowAgainCheck->Visible = FLAGSET(Options, coDoNotShowAgain);
  RemoteDirectoryEdit->Visible = false;
  LocalDirectoryEdit->Visible = false;
  DirectoryEdit->Visible = FLAGCLEAR(Options, coTemp);
  EnableControl(DirectoryEdit, FLAGCLEAR(Options, coDisableDirectory));
  EnableControl(DirectoryLabel, DirectoryEdit->Enabled);
  EnableControl(LocalDirectoryBrowseButton, DirectoryEdit->Enabled);
  DirectoryLabel->FocusControl = DirectoryEdit;
  CopyParamsFrame->Direction = !ToRemote ? pdToLocal : pdToRemote;
  PresetsButton->Visible = FLAGCLEAR(Options, coDoNotUsePresets);

  AdjustTransferControls();

  LocalDirectoryBrowseButton->Visible = !ToRemote &&
    ((Options & coTemp) == 0);

  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::SetToRemote(bool value)
{
  if (FToRemote != value)
  {
    AnsiString ADirectory = DirectoryEdit->Text;
    FToRemote = value;
    DirectoryEdit->Text = ADirectory;

    AdjustControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::SetOptions(int value)
{
  if (Options != value)
  {
    FOptions = value;

    AdjustControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::SetOutputOptions(int value)
{
  if (OutputOptions != value)
  {
    SaveSettingsCheck->Checked = FLAGSET(FOutputOptions, cooSaveSettings);
    NeverShowAgainCheck->Checked = FLAGSET(FOutputOptions, cooDoNotShowAgain);
    FOutputOptions = (value & ~(cooDoNotShowAgain | cooSaveSettings));
  }
}
//---------------------------------------------------------------------------
int __fastcall TCopyDialog::GetOutputOptions()
{
  return FOutputOptions |
    FLAGMASK(SaveSettingsCheck->Checked, cooSaveSettings) |
    FLAGMASK(NeverShowAgainCheck->Checked, cooDoNotShowAgain);
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::SetCopyParamAttrs(int value)
{
  CopyParamsFrame->CopyParamAttrs = value;
}
//---------------------------------------------------------------------------
int __fastcall TCopyDialog::GetCopyParamAttrs()
{
  return CopyParamsFrame->CopyParamAttrs;
}
//---------------------------------------------------------------------------
THistoryComboBox * __fastcall TCopyDialog::GetDirectoryEdit()
{
  return ToRemote ? RemoteDirectoryEdit : LocalDirectoryEdit;
}
//---------------------------------------------------------------------------
bool __fastcall TCopyDialog::RemotePaths()
{
  return (ToRemote || FLAGSET(FOutputOptions, cooRemoteTransfer));
}
//---------------------------------------------------------------------------
AnsiString __fastcall TCopyDialog::GetFileMask()
{
  return ExtractFileName(DirectoryEdit->Text, RemotePaths());
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::SetParams(const TGUICopyParamType & value)
{
  FParams = value;
  CopyParamsFrame->Params = value;
  DirectoryEdit->Text = Directory + FParams.FileMask;
  QueueCheck->Checked = FParams.Queue;
  QueueNoConfirmationCheck->Checked = FParams.QueueNoConfirmation;
  NewerOnlyCheck->Checked = FLAGCLEAR(Options, coDisableNewerOnly) && FParams.NewerOnly;
}
//---------------------------------------------------------------------------
TGUICopyParamType __fastcall TCopyDialog::GetParams()
{
  // overwrites TCopyParamType files only
  FParams = CopyParamsFrame->Params;
  FParams.FileMask = GetFileMask();
  FParams.Queue = QueueCheck->Checked;
  FParams.QueueNoConfirmation = QueueNoConfirmationCheck->Checked;
  FParams.NewerOnly = FLAGCLEAR(Options, coDisableNewerOnly) && NewerOnlyCheck->Checked;
  return FParams;
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::SetDirectory(AnsiString value)
{
  if (!value.IsEmpty())
  {
    value = RemotePaths() ?
      UnixIncludeTrailingBackslash(value) : IncludeTrailingBackslash(value);
  }
  DirectoryEdit->Text = value + GetFileMask();
}
//---------------------------------------------------------------------------
AnsiString __fastcall TCopyDialog::GetDirectory()
{
  assert(DirectoryEdit);

  AnsiString Result = DirectoryEdit->Text;
  if (RemotePaths())
  {
    Result = UnixExtractFilePath(Result);
    if (!Result.IsEmpty())
    {
      Result = UnixIncludeTrailingBackslash(Result);
    }
  }
  else
  {
    Result = ExtractFilePath(Result);
    if (!Result.IsEmpty())
    {
      Result = IncludeTrailingBackslash(Result);
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::SetFileList(TStrings * value)
{
  if (FFileList != value)
  {
    FFileList = value;
    AdjustControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::UpdateControls()
{
  if (!ToRemote && FLAGSET(Options, coAllowRemoteTransfer))
  {
    AnsiString Directory = DirectoryEdit->Text;
    bool RemoteTransfer = (Directory.Pos("\\") == 0) && (Directory.Pos("/") > 0);
    if (RemoteTransfer != FLAGSET(FOutputOptions, cooRemoteTransfer))
    {
      FOutputOptions =
        (FOutputOptions & ~cooRemoteTransfer) |
        FLAGMASK(RemoteTransfer, cooRemoteTransfer);
      AdjustTransferControls();
    }
  }

  bool RemoteTransfer = FLAGSET(FOutputOptions, cooRemoteTransfer);
  EnableControl(QueueCheck,
    ((Options & (coDisableQueue | coTemp)) == 0) && !RemoteTransfer);
  EnableControl(QueueNoConfirmationCheck,
    (((Options & coTemp) == 0) && QueueCheck->Checked) && !RemoteTransfer);
  QueueNoConfirmationCheck->Visible = MoreButton->Expanded;
  EnableControl(SaveSettingsCheck, FLAGCLEAR(Options, coDisableSaveSettings) &&
    !RemoteTransfer);
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::SetMove(bool value)
{
  if (Move != value)
  {
    FMove = value;
    AdjustControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::FormShow(TObject * /*Sender*/)
{
  assert(FileList && (FileList->Count > 0));
  if (DirectoryEdit->Enabled && DirectoryEdit->Visible)
  {
    DirectoryEdit->SetFocus();
  }
  else
  if (MoreButton->Expanded)
  {
    MorePanel->SetFocus();
  }
  else
  {
    CopyButton->SetFocus();
  }
  UpdateControls();

  InstallPathWordBreakProc(RemoteDirectoryEdit);
  InstallPathWordBreakProc(LocalDirectoryEdit);
}
//---------------------------------------------------------------------------
bool __fastcall TCopyDialog::Execute()
{
  // at start assume that copy param is current preset
  FPreset = GUIConfiguration->CopyParamCurrent;
  DirectoryEdit->Items = CustomWinConfiguration->History[
    ToRemote ? "RemoteTarget" : "LocalTarget"];
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
      if (FLAGSET(OutputOptions, cooSaveSettings) &&
          FLAGCLEAR(Options, coDisableSaveSettings))
      {
        GUIConfiguration->DefaultCopyParam = Params;
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
    if (!RemotePaths() && ((Options & coTemp) == 0))
    {
      AnsiString Dir = Directory;
      AnsiString Drive = ExtractFileDrive(Dir);
      if (!DirectoryExists(Dir))
      {
        if (MessageDialog(FMTLOAD(CREATE_LOCAL_DIRECTORY, (Dir)),
              qtConfirmation, qaOK | qaCancel, HELP_NONE) != qaCancel)
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

    if (CanClose)
    {
      ExitActiveControl(this);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::LocalDirectoryBrowseButtonClick(
  TObject * /*Sender*/)
{
  assert(!ToRemote);
  AnsiString ADirectory;
  // if we are duplicating, we have remote path there
  if (!RemotePaths())
  {
    ADirectory = Directory;
  }

  if (SelectDirectory(ADirectory, LoadStr(SELECT_LOCAL_DIRECTORY), true))
  {
    Directory = ADirectory;
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::ControlChange(TObject * /*Sender*/)
{
  UpdateControls();
  ResetSystemSettings(this);
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::PresetsButtonClick(TObject * /*Sender*/)
{
  TCopyParamType Param = Params;
  CopyParamListPopup(
    PresetsButton->ClientToScreen(TPoint(0, PresetsButton->Height)),
    FPresetsMenu, Params, FPreset, CopyParamClick, cplNone);
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::CopyParamClick(TObject * Sender)
{
  TCopyParamType Param = Params;
  if (CopyParamListPopupClick(Sender, Param, FPreset, CopyParamAttrs))
  {
    Params = Param;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
