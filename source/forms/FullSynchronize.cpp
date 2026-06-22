//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>

#include "WinInterface.h"
#include "FullSynchronize.h"
#include "CopyParams.h"
#include "VCLCommon.h"

#include <CoreMain.h>
#include <Configuration.h>
#include <TextsWin.h>
#include <HelpWin.h>
#include <GUITools.h>
#include <Terminal.h>
#include <CustomWinConfiguration.h>
#include <Tools.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "HistoryComboBox"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
bool __fastcall DoFullSynchronizeDialog(TSynchronizeMode & Mode, int & Params,
  UnicodeString & LocalDirectory, UnicodeString & RemoteDirectory,
  TCopyParamType * CopyParams, bool & SaveSettings, bool & SaveMode, int Options,
  const TUsableCopyParamAttrs & CopyParamAttrs, TFullSynchronizeInNewWindow OnFullSynchronizeInNewWindow,
  int AutoSubmit)
{
  bool Result;
  TFullSynchronizeDialog * Dialog = SafeFormCreate<TFullSynchronizeDialog>();
  try
  {
    Dialog->Init(Options, CopyParamAttrs, OnFullSynchronizeInNewWindow);
    Dialog->Mode = Mode;
    Dialog->Params = Params;
    Dialog->LocalDirectory = LocalDirectory;
    Dialog->RemoteDirectory = RemoteDirectory;
    Dialog->CopyParams = *CopyParams;
    Dialog->SaveSettings = SaveSettings;
    Dialog->SaveMode = SaveMode;
    if (AutoSubmit > 0)
    {
      InitiateDialogTimeout(Dialog, AutoSubmit * MSecsPerSec, Dialog->OkButton);
    }
    Result = Dialog->Execute();
    if (Result)
    {
      Mode = Dialog->Mode;
      Params = Dialog->Params;
      LocalDirectory = Dialog->LocalDirectory;
      RemoteDirectory = Dialog->RemoteDirectory;
      *CopyParams = Dialog->CopyParams;
      SaveSettings = Dialog->SaveSettings;
      SaveMode = Dialog->SaveMode;
    }
  }
  __finally
  {
    delete Dialog;
  }
  return Result;
}
//---------------------------------------------------------------------------
__fastcall TFullSynchronizeDialog::TFullSynchronizeDialog(TComponent* Owner)
  : TForm(Owner)
{
  UseSystemSettings(this);
  FParams = 0;
  FSaveMode = false;
  FOptions = 0;
  FOnFullSynchronizeInNewWindow = NULL;
  FPresetsMenu = new TPopupMenu(this);
  FSynchronizeBySizeCaption = SynchronizeBySizeCheck->Caption;
  HotTrackLabel(CopyParamLabel);
  LoadDialogImage(Image, L"Synchronize directories");
}
//---------------------------------------------------------------------------
__fastcall TFullSynchronizeDialog::~TFullSynchronizeDialog()
{
  delete FPresetsMenu;
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::Init(
  int Options, const TUsableCopyParamAttrs & CopyParamAttrs, TFullSynchronizeInNewWindow OnFullSynchronizeInNewWindow)
{
  FOptions = Options;
  if (FLAGSET(Options, fsoDisableTimestamp) &&
      SynchronizeTimestampsButton->Checked)
  {
    SynchronizeFilesButton->Checked = true;
  }
  FCopyParamAttrs = CopyParamAttrs;
  FOnFullSynchronizeInNewWindow = OnFullSynchronizeInNewWindow;
  DebugAssert(FOnFullSynchronizeInNewWindow != NULL);
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::UpdateControls()
{
  EnableControl(SynchronizeTimestampsButton, FLAGCLEAR(FOptions, fsoDisableTimestamp));
  if (SynchronizeTimestampsButton->Checked)
  {
    SynchronizeExistingOnlyCheck->Checked = true;
    SynchronizeDeleteCheck->Checked = false;
    SynchronizeByTimeCheck->Checked = true;
    SynchronizeByChecksumCheck->Checked = false;
  }
  if (SynchronizeBothButton->Checked)
  {
    SynchronizeByTimeCheck->Checked = true;
    SynchronizeBySizeCheck->Checked = false;
    SynchronizeByChecksumCheck->Checked = false;
    if (MirrorFilesButton->Checked)
    {
      SynchronizeFilesButton->Checked = true;
    }
  }
  EnableControl(MirrorFilesButton, !SynchronizeBothButton->Checked);
  EnableControl(SynchronizeDeleteCheck, !SynchronizeBothButton->Checked &&
    !SynchronizeTimestampsButton->Checked);
  EnableControl(SynchronizeExistingOnlyCheck, !SynchronizeTimestampsButton->Checked);
  EnableControl(SynchronizeByTimeCheck, !SynchronizeBothButton->Checked &&
    !SynchronizeTimestampsButton->Checked);
  EnableControl(SynchronizeBySizeCheck, !SynchronizeBothButton->Checked);
  EnableControl(SynchronizeByChecksumCheck,
    FLAGCLEAR(FOptions, fsoDisableByChecksum) && !SynchronizeBothButton->Checked && !SynchronizeTimestampsButton->Checked);
  EnableControl(SynchronizeSelectedOnlyCheck, FLAGSET(FOptions, fsoAllowSelectedOnly));

  EnableControl(OkButton, !LocalDirectoryEdit->Text.IsEmpty() &&
    !RemoteDirectoryEdit->Text.IsEmpty());

  UnicodeString InfoStr = FCopyParams.GetInfoStr(L"; ", ActualCopyParamAttrs());
  SetLabelHintPopup(CopyParamLabel, InfoStr);

  SynchronizeBySizeCheck->Caption = SynchronizeTimestampsButton->Checked ?
    LoadStr(SYNCHRONIZE_SAME_SIZE) : UnicodeString(FSynchronizeBySizeCaption);

  TransferSettingsButton->Style =
    FLAGCLEAR(FOptions, fsoDoNotUsePresets) ?
      TCustomButton::bsSplitButton : TCustomButton::bsPushButton;

  OkButton->Style = AllowStartInNewWindow() ? TCustomButton::bsSplitButton : TCustomButton::bsPushButton;
  StartInNewWindowItem->Enabled = CanStartInNewWindow();
}
//---------------------------------------------------------------------------
int __fastcall TFullSynchronizeDialog::ActualCopyParamAttrs()
{
  int Result;
  if (SynchronizeTimestampsButton->Checked)
  {
    Result = cpaIncludeMaskOnly;
  }
  else
  {
    switch (Mode)
    {
      case smRemote:
        Result = FCopyParamAttrs.Upload;
        break;

      case smLocal:
        Result = FCopyParamAttrs.Download;
        break;

      default:
        DebugFail();
        //fallthru
      case smBoth:
        Result = FCopyParamAttrs.General;
        break;
    }
  }
  return
    Result |
    FLAGMASK(SynchronizeByTimeCheck->Checked, cpaNoPreserveTime) |
    cpaNoNewerOnly;
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::ControlChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
bool __fastcall TFullSynchronizeDialog::Execute()
{
  // at start assume that copy param is current preset
  FPreset = GUIConfiguration->CopyParamCurrent;
  LocalDirectoryEdit->Items = CustomWinConfiguration->History[L"LocalDirectory"];
  RemoteDirectoryEdit->Items = CustomWinConfiguration->History[L"RemoteDirectory"];
  bool Result = (ShowModal() == DefaultResult(this));
  if (Result)
  {
    Submitted();
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::Submitted()
{
  LocalDirectoryEdit->SaveToHistory();
  CustomWinConfiguration->History[L"LocalDirectory"] = LocalDirectoryEdit->Items;
  RemoteDirectoryEdit->SaveToHistory();
  CustomWinConfiguration->History[L"RemoteDirectory"] = RemoteDirectoryEdit->Items;
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::SetRemoteDirectory(const UnicodeString value)
{
  RemoteDirectoryEdit->Text = value;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TFullSynchronizeDialog::GetRemoteDirectory()
{
  return RemoteDirectoryEdit->Text;
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::SetLocalDirectory(const UnicodeString value)
{
  LocalDirectoryEdit->Text = value;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TFullSynchronizeDialog::GetLocalDirectory()
{
  return LocalDirectoryEdit->Text;
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::SetMode(TSynchronizeMode value)
{
  FOrigMode = value;
  switch (value)
  {
    case smRemote:
       SynchronizeRemoteButton->Checked = true;
       break;

    case smLocal:
       SynchronizeLocalButton->Checked = true;
       break;

    case smBoth:
       SynchronizeBothButton->Checked = true;
       break;

    default:
      DebugFail();
  }
}
//---------------------------------------------------------------------------
TSynchronizeMode __fastcall TFullSynchronizeDialog::GetMode()
{
  if (SynchronizeRemoteButton->Checked)
  {
    return smRemote;
  }
  else if (SynchronizeLocalButton->Checked)
  {
    return smLocal;
  }
  else
  {
    DebugAssert(SynchronizeBothButton->Checked);
    return smBoth;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::SetParams(int value)
{
  FParams = value &
    ~(TTerminal::spDelete | TTerminal::spExistingOnly | TTerminal::spPreviewChanges | TTerminal::spTimestamp |
      TTerminal::spNotByTime | TTerminal::spBySize | TTerminal::spSelectedOnly | TTerminal::spMirror |
      TTerminal::spCaseSensitive | TTerminal::spByChecksum);
  SynchronizeDeleteCheck->Checked = FLAGSET(value, TTerminal::spDelete);
  SynchronizeExistingOnlyCheck->Checked = FLAGSET(value, TTerminal::spExistingOnly);
  SynchronizePreviewChangesCheck->Checked = FLAGSET(value, TTerminal::spPreviewChanges);
  SynchronizeSelectedOnlyCheck->Checked = FLAGSET(value, TTerminal::spSelectedOnly);
  if (FLAGSET(value, TTerminal::spTimestamp) && FLAGCLEAR(FOptions, fsoDisableTimestamp))
  {
    SynchronizeTimestampsButton->Checked = true;
  }
  else if (FLAGSET(value, TTerminal::spMirror))
  {
    MirrorFilesButton->Checked = true;
  }
  else
  {
    SynchronizeFilesButton->Checked = true;
  }
  SynchronizeByTimeCheck->Checked = FLAGCLEAR(value, TTerminal::spNotByTime);
  SynchronizeBySizeCheck->Checked = FLAGSET(value, TTerminal::spBySize);
  SynchronizeByChecksumCheck->Checked = FLAGSET(value, TTerminal::spByChecksum);
  SynchronizeCaseSensitiveCheck->Checked = FLAGSET(value, TTerminal::spCaseSensitive);
  UpdateControls();
}
//---------------------------------------------------------------------------
int __fastcall TFullSynchronizeDialog::GetParams()
{
  return FParams |
    FLAGMASK(SynchronizeDeleteCheck->Checked, TTerminal::spDelete) |
    FLAGMASK(SynchronizeExistingOnlyCheck->Checked, TTerminal::spExistingOnly) |
    FLAGMASK(SynchronizePreviewChangesCheck->Checked, TTerminal::spPreviewChanges) |
    FLAGMASK(SynchronizeSelectedOnlyCheck->Checked, TTerminal::spSelectedOnly) |
    FLAGMASK(SynchronizeTimestampsButton->Checked && FLAGCLEAR(FOptions, fsoDisableTimestamp),
      TTerminal::spTimestamp) |
    FLAGMASK(MirrorFilesButton->Checked, TTerminal::spMirror) |
    FLAGMASK(!SynchronizeByTimeCheck->Checked, TTerminal::spNotByTime) |
    FLAGMASK(SynchronizeBySizeCheck->Checked, TTerminal::spBySize) |
    FLAGMASK(SynchronizeByChecksumCheck->Enabled && SynchronizeByChecksumCheck->Checked, TTerminal::spByChecksum) |
    FLAGMASK(SynchronizeCaseSensitiveCheck->Checked, TTerminal::spCaseSensitive);
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::LocalDirectoryBrowseButtonClick(
      TObject * /*Sender*/)
{
  UnicodeString Directory = LocalDirectoryEdit->Text;
  if (SelectDirectory(Directory, LoadStr(SELECT_LOCAL_DIRECTORY), false))
  {
    LocalDirectoryEdit->Text = Directory;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::SetSaveSettings(bool value)
{
  SaveSettingsCheck->Checked = value;
}
//---------------------------------------------------------------------------
bool __fastcall TFullSynchronizeDialog::GetSaveSettings()
{
  return SaveSettingsCheck->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::CopyParamListPopup(TRect R, int AdditionalOptions)
{
  // We pass in FCopyParams, although it may not be the exact copy param
  // that will be used (because of Preservetime). The reason is to
  // display checkbox next to user-selected preset
  ::CopyParamListPopup(
    R, FPresetsMenu, FCopyParams, FPreset, CopyParamClick,
    AdditionalOptions,
    ActualCopyParamAttrs());
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::TransferSettingsButtonClick(
  TObject * /*Sender*/)
{
  CopyParamGroupClick(NULL);
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::CopyParamClick(TObject * Sender)
{
  DebugAssert(FLAGCLEAR(FOptions, fsoDoNotUsePresets));
  // PreserveTime is forced for some settings, but avoid hard-setting it until
  // user really confirms it on custom dialog
  TCopyParamType ACopyParams = CopyParams;
  if (CopyParamListPopupClick(Sender, ACopyParams, FPreset,
        ActualCopyParamAttrs()) > 0)
  {
    FCopyParams = ACopyParams;
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::FormShow(TObject * /*Sender*/)
{
  InstallPathWordBreakProc(LocalDirectoryEdit);
  InstallPathWordBreakProc(RemoteDirectoryEdit);

  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::FormCloseQuery(TObject * /*Sender*/,
  bool & CanClose)
{
  if ((ModalResult == DefaultResult(this)) &&
      SaveSettings && (FOrigMode != Mode) && !FSaveMode)
  {
    switch (MessageDialog(LoadStr(SAVE_SYNCHRONIZE_MODE2),
          qtConfirmation, qaYes | qaNo | qaCancel, HELP_SYNCHRONIZE_SAVE_MODE))
    {
      case qaYes:
        FSaveMode = true;
        break;

      case qaCancel:
        CanClose = false;
        break;
    }
  }
}
//---------------------------------------------------------------------------
TCopyParamType __fastcall TFullSynchronizeDialog::GetCopyParams()
{
  TCopyParamType Result = FCopyParams;
  // when synchronizing by time, we force preserving time,
  // otherwise it does not make any sense
  if (FLAGCLEAR(Params, TTerminal::spNotByTime))
  {
    Result.PreserveTime = true;
  }
  Result.NewerOnly = false;
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::SetCopyParams(const TCopyParamType & value)
{
  FCopyParams = value;
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::CopyParamGroupContextPopup(
  TObject * /*Sender*/, TPoint & MousePos, bool & Handled)
{
  if (FLAGCLEAR(FOptions, fsoDoNotUsePresets))
  {
    CopyParamListPopup(CalculatePopupRect(CopyParamGroup, MousePos), cplCustomizeDefault);
    Handled = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::CopyParamGroupClick(
  TObject * /*Sender*/)
{
  // PreserveTime is forced for some settings, but avoid hard-setting it until
  // user really confirms it on custom dialog
  TCopyParamType ACopyParams = CopyParams;
  if (DoCopyParamCustomDialog(ACopyParams, ActualCopyParamAttrs()))
  {
    FCopyParams = ACopyParams;
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::TransferSettingsButtonDropDownClick(TObject * /*Sender*/)
{
  CopyParamListPopup(CalculatePopupRect(TransferSettingsButton), cplCustomizeDefault);
}
//---------------------------------------------------------------------------
bool __fastcall TFullSynchronizeDialog::AllowStartInNewWindow()
{
  return !IsMainFormLike(this);
}
//---------------------------------------------------------------------------
bool __fastcall TFullSynchronizeDialog::CanStartInNewWindow()
{
  return
    AllowStartInNewWindow() &&
    (!SynchronizeSelectedOnlyCheck->Enabled || !SynchronizeSelectedOnlyCheck->Checked);
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::Start1Click(TObject *)
{
  OkButton->Click();
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::OkButtonDropDownClick(TObject *)
{
  MenuPopup(OkMenu, OkButton);
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::OkButtonClick(TObject *)
{
  if (OpenInNewWindow())
  {
    if (CanStartInNewWindow())
    {
      StartInNewWindow();
      ModalResult = mrCancel;
    }
    else
    {
      Beep();
      ModalResult = mrNone;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::StartInNewWindow()
{
  Submitted();
  TCopyParamType ACopyParams = CopyParams;
  FOnFullSynchronizeInNewWindow(Mode, Params, LocalDirectory, RemoteDirectory, &ACopyParams);
  Close();
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::StartInNewWindowItemClick(TObject *)
{
  StartInNewWindow();
}
//---------------------------------------------------------------------------
