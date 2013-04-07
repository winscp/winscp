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
#pragma link "Rights"
#pragma link "CopyParams"
#pragma link "HistoryComboBox"
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------------
bool __fastcall DoCopyDialog(bool ToRemote,
  bool Move, TStrings * FileList, UnicodeString & TargetDirectory,
  TGUICopyParamType * Params, int Options, int CopyParamAttrs, int * OutputOptions)
{
  bool Result;
  TCopyDialog *CopyDialog = new TCopyDialog(Application, ToRemote, Move, FileList, Options, CopyParamAttrs);
  try
  {
    if (FLAGSET(CopyParamAttrs, cpaNoTransferMode))
    {
      // If local and remote EOL types are the same, there is no need
      // for ASCII (or Automatic) mode
      Params->TransferMode = tmBinary;
    }
    if (OutputOptions != NULL)
    {
      CopyDialog->OutputOptions = *OutputOptions;
    }
    CopyDialog->Directory = TargetDirectory;
    CopyDialog->Params = *Params;
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
__fastcall TCopyDialog::TCopyDialog(
  TComponent* Owner, bool ToRemote, bool Move, TStrings * FileList, int Options, int CopyParamAttrs)
        : TForm(Owner)
{
  FToRemote = ToRemote;
  FMove = Move;
  FOptions = Options;
  FCopyParamAttrs = CopyParamAttrs;
  FFileList = FileList;

  FOutputOptions = 0;

  AdjustControls();

  FPresetsMenu = new TPopupMenu(this);

  HotTrackLabel(CopyParamLabel);

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
  if (FFileList && FFileList->Count)
  {
    if (!FToRemote && !FMove && FLAGSET(FOutputOptions, cooRemoteTransfer))
    {
      UnicodeString Label;
      if (FFileList->Count == 1)
      {
        UnicodeString FileName;
        if (!FToRemote) FileName = UnixExtractFileName(FFileList->Strings[0]);
          else FileName = ExtractFileName(FFileList->Strings[0]);
        Label = FMTLOAD(REMOTE_COPY_FILE, (FileName));
      }
      else
      {
        Label = FMTLOAD(REMOTE_COPY_FILES, (FFileList->Count));
      }

      DirectoryLabel->Caption = Label;
    }
    else
    {
      UnicodeString TransferStr =
        LoadStr(RemotePaths() ? COPY_COPY_TOREMOTE : COPY_COPY_TOLOCAL);
      // currently the copy dialog is shown when downloading to temp folder
      // only for drag&drop downloads, for we dare to display d&d specific prompt
      UnicodeString DirectionStr =
        LoadStr(((FOptions & coTemp) != 0) ? COPY_TODROP :
          (RemotePaths() ? COPY_TOREMOTE : COPY_TOLOCAL));

      if (FFileList->Count == 1)
      {
        UnicodeString FileName;
        if (!FToRemote) FileName = UnixExtractFileName(FFileList->Strings[0]);
          else FileName = ExtractFileName(FFileList->Strings[0]);
        DirectoryLabel->Caption = FMTLOAD((FMove ? MOVE_FILE : COPY_FILE),
          (TransferStr, FileName, DirectionStr));
      }
      else
      {
        DirectoryLabel->Caption = FMTLOAD((FMove ? MOVE_FILES : COPY_FILES),
          (TransferStr, FFileList->Count, DirectionStr));
      }
    }
  }

  TImage * Image;
  if (!FMove)
  {
    if (!FToRemote && FLAGSET(FOutputOptions, cooRemoteTransfer))
    {
      Caption = LoadStr(REMOTE_COPY_TITLE);
      Image = CopyImage;
    }
    else
    {
      if (RemotePaths())
      {
        Caption = LoadStr(COPY_COPY_TOREMOTE_CAPTION);
        Image = CopyUploadImage;
      }
      else
      {
        Caption = LoadStr(COPY_COPY_TOLOCAL_CAPTION);
        Image = CopyDownloadImage;
      }
    }
  }
  else
  {
    if (!FToRemote && FLAGSET(FOutputOptions, cooRemoteTransfer))
    {
      Caption = LoadStr(COPY_MOVE_CAPTION);
      Image = MoveImage;
    }
    else
    {
      if (RemotePaths())
      {
        Caption = LoadStr(COPY_MOVE_TOREMOTE_CAPTION);
        Image = MoveUploadImage;
      }
      else
      {
        Caption = LoadStr(COPY_MOVE_TOLOCAL_CAPTION);
        Image = MoveDownloadImage;
      }
    }
  }

  CopyImage->Visible = (Image == CopyImage) || (Image == NULL);
  MoveImage->Visible = (Image == MoveImage);
  CopyUploadImage->Visible = (Image == CopyUploadImage);
  CopyDownloadImage->Visible = (Image == CopyDownloadImage);
  MoveUploadImage->Visible = (Image == MoveUploadImage);
  MoveDownloadImage->Visible = (Image == MoveDownloadImage);

  bool RemoteTransfer = FLAGSET(FOutputOptions, cooRemoteTransfer);
  assert(FLAGSET(FOptions, coAllowRemoteTransfer) || !RemoteTransfer);

  EnableControl(TransferSettingsButton, !RemoteTransfer);
  EnableControl(CopyParamGroup, !RemoteTransfer);
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::AdjustControls()
{
  RemoteDirectoryEdit->Visible = false;
  LocalDirectoryEdit->Visible = false;
  DirectoryEdit->Visible = FLAGCLEAR(FOptions, coTemp);
  EnableControl(DirectoryEdit, FLAGCLEAR(FOptions, coDisableDirectory));
  EnableControl(DirectoryLabel, DirectoryEdit->Enabled);
  EnableControl(LocalDirectoryBrowseButton, DirectoryEdit->Enabled);
  DirectoryLabel->FocusControl = DirectoryEdit;

  UnicodeString QueueLabel = LoadStr(COPY_BACKGROUND);
  if (FLAGCLEAR(FOptions, coNoQueue))
  {
    QueueLabel = FMTLOAD(COPY_QUEUE, (QueueLabel));
  }
  QueueCheck2->Caption = QueueLabel;

  AdjustTransferControls();

  LocalDirectoryBrowseButton->Visible = !FToRemote &&
    FLAGCLEAR(FOptions, coTemp);

  if (FLAGCLEAR(FOptions, coDoNotShowAgain))
  {
    NeverShowAgainCheck->Visible = false;
    ClientHeight = NeverShowAgainCheck->Top;
  }

  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::SetOutputOptions(int value)
{
  if (OutputOptions != value)
  {
    FSaveSettings = FLAGSET(value, cooSaveSettings);
    NeverShowAgainCheck->Checked = FLAGSET(value, cooDoNotShowAgain);
    FOutputOptions = (value & ~(cooDoNotShowAgain | cooSaveSettings));
  }
}
//---------------------------------------------------------------------------
int __fastcall TCopyDialog::GetOutputOptions()
{
  return FOutputOptions |
    FLAGMASK(FSaveSettings, cooSaveSettings) |
    FLAGMASK(NeverShowAgainCheck->Checked, cooDoNotShowAgain);
}
//---------------------------------------------------------------------------
THistoryComboBox * __fastcall TCopyDialog::GetDirectoryEdit()
{
  return FToRemote ? RemoteDirectoryEdit : LocalDirectoryEdit;
}
//---------------------------------------------------------------------------
bool __fastcall TCopyDialog::RemotePaths()
{
  return (FToRemote || FLAGSET(FOutputOptions, cooRemoteTransfer));
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCopyDialog::GetFileMask()
{
  return ExtractFileName(DirectoryEdit->Text, RemotePaths());
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::SetParams(const TGUICopyParamType & value)
{
  FParams = value;
  FCopyParams = value;
  DirectoryEdit->Text = Directory + FParams.FileMask;
  QueueCheck2->Checked = FParams.Queue;
  QueueIndividuallyCheck->Checked = FParams.QueueIndividually;
  UpdateControls();
}
//---------------------------------------------------------------------------
TGUICopyParamType __fastcall TCopyDialog::GetParams()
{
  // overwrites TCopyParamType fields only
  FParams = FCopyParams;
  FParams.FileMask = GetFileMask();
  FParams.Queue = QueueCheck2->Checked;
  FParams.QueueIndividually = QueueIndividuallyCheck->Checked;
  return FParams;
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::SetDirectory(UnicodeString value)
{
  if (!value.IsEmpty())
  {
    value = RemotePaths() ?
      UnicodeString(UnixIncludeTrailingBackslash(value)) : IncludeTrailingBackslash(value);
  }
  DirectoryEdit->Text = value + GetFileMask();
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCopyDialog::GetDirectory()
{
  assert(DirectoryEdit);

  UnicodeString Result = DirectoryEdit->Text;
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
void __fastcall TCopyDialog::UpdateControls()
{
  if (!FToRemote && FLAGSET(FOptions, coAllowRemoteTransfer))
  {
    UnicodeString Directory = DirectoryEdit->Text;
    bool RemoteTransfer = (Directory.Pos(L"\\") == 0) && (Directory.Pos(L"/") > 0);
    if (RemoteTransfer != FLAGSET(FOutputOptions, cooRemoteTransfer))
    {
      FOutputOptions =
        (FOutputOptions & ~cooRemoteTransfer) |
        FLAGMASK(RemoteTransfer, cooRemoteTransfer);
      AdjustTransferControls();
    }
  }

  UnicodeString InfoStr = FCopyParams.GetInfoStr(L"; ", FCopyParamAttrs);
  CopyParamLabel->Caption = InfoStr;
  CopyParamLabel->Hint = InfoStr;
  CopyParamLabel->ShowHint =
    (CopyParamLabel->Canvas->TextWidth(InfoStr) > (CopyParamLabel->Width * 3 / 2));

  bool RemoteTransfer = FLAGSET(FOutputOptions, cooRemoteTransfer);
  EnableControl(QueueCheck2,
    ((FOptions & (coDisableQueue | coTemp)) == 0) && !RemoteTransfer);
  QueueIndividuallyCheck->Visible =
    FLAGCLEAR(FOptions, coNoQueueIndividually) &&
    QueueCheck2->Enabled && QueueCheck2->Checked &&
    (FFileList != NULL) && (FFileList->Count > 1);

  TransferSettingsButton->Style =
    FLAGCLEAR(FOptions, coDoNotUsePresets) ?
      TCustomButton::bsSplitButton : TCustomButton::bsPushButton;
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::FormShow(TObject * /*Sender*/)
{
  assert(FFileList && (FFileList->Count > 0));
  if (DirectoryEdit->Enabled && DirectoryEdit->Visible)
  {
    ActiveControl = DirectoryEdit;
  }
  else
  {
    ActiveControl = OkButton;
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
    FToRemote ? L"RemoteTarget" : L"LocalTarget"];
  bool Result = (ShowModal() == mrOk);
  if (Result)
  {
    Configuration->BeginUpdate();
    try
    {
      if (FLAGSET(OutputOptions, cooSaveSettings) &&
          FLAGCLEAR(FOptions, coDisableSaveSettings))
      {
        GUIConfiguration->DefaultCopyParam = Params;
      }
      DirectoryEdit->SaveToHistory();
      CustomWinConfiguration->History[FToRemote ?
        L"RemoteTarget" : L"LocalTarget"] = DirectoryEdit->Items;
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
    if (!RemotePaths() && ((FOptions & coTemp) == 0))
    {
      UnicodeString Dir = Directory;
      UnicodeString Drive = ExtractFileDrive(Dir);
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
  assert(!FToRemote);
  UnicodeString ADirectory;
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
void __fastcall TCopyDialog::TransferSettingsButtonClick(TObject * /*Sender*/)
{
  if (FLAGCLEAR(FOptions, coDoNotUsePresets) && !SupportsSplitButton())
  {
    CopyParamListPopup(
      TransferSettingsButton->ClientToScreen(TPoint(0, TransferSettingsButton->Height)),
      0);
  }
  else
  {
    CopyParamGroupClick(NULL);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::CopyParamClick(TObject * Sender)
{
  TCopyParamType Param = Params;
  bool PrevSaveSettings = FSaveSettings;
  if (CopyParamListPopupClick(Sender, Param, FPreset, FCopyParamAttrs, &FSaveSettings))
  {
    Params = Param;
  }
  else
  {
    UpdateControls();
  }

  if (PrevSaveSettings && !FSaveSettings)
  {
    NeverShowAgainCheck->Checked = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::CopyParamGroupClick(TObject * /*Sender*/)
{
  if (CopyParamGroup->Enabled)
  {
    if (DoCopyParamCustomDialog(FCopyParams, FCopyParamAttrs))
    {
      UpdateControls();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::CopyParamGroupContextPopup(TObject * /*Sender*/,
  TPoint & MousePos, bool & Handled)
{
  if (FLAGCLEAR(FOptions, coDoNotUsePresets))
  {
    CopyParamListPopup(CopyParamGroup->ClientToScreen(MousePos), cplCustomizeDefault);
    Handled = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::CopyParamListPopup(TPoint P, int AdditionalOptions)
{
  bool RemoteTransfer = FLAGSET(FOutputOptions, cooRemoteTransfer);

  ::CopyParamListPopup(P, FPresetsMenu,
    FCopyParams, FPreset, CopyParamClick,
    cplCustomize | AdditionalOptions |
      FLAGMASK(
          FLAGCLEAR(FOptions, coDisableSaveSettings) && !RemoteTransfer,
        cplSaveSettings),
    FCopyParamAttrs,
    FSaveSettings);
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::TransferSettingsButtonDropDownClick(TObject * /*Sender*/)
{
  CopyParamListPopup(
    TransferSettingsButton->ClientToScreen(TPoint(0, TransferSettingsButton->Height)),
    cplCustomizeDefault);
}
//---------------------------------------------------------------------------
void __fastcall TCopyDialog::NeverShowAgainCheckClick(TObject * /*Sender*/)
{
  FSaveSettings = NeverShowAgainCheck->Checked;
  UpdateControls();
}
//---------------------------------------------------------------------------
