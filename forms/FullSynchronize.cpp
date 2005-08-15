//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>

#include "WinInterface.h"
#include "FullSynchronize.h"
#include "CopyParams.h"
#include "VCLCommon.h"

#include <ScpMain.h>
#include <Configuration.h>
#include <TextsWin.h>
#include <HelpWin.h>
#include <CustomWinConfiguration.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "XPThemes"
#pragma link "HistoryComboBox"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
bool __fastcall DoFullSynchronizeDialog(TSynchronizeMode & Mode, int & Params,
  AnsiString & LocalDirectory, AnsiString & RemoteDirectory,
  TCopyParamType * CopyParams, bool & SaveSettings, bool & SaveMode, int Options)
{
  bool Result;
  TFullSynchronizeDialog * Dialog = new TFullSynchronizeDialog(Application);
  try
  {
    Dialog->Mode = Mode;
    Dialog->Options = Options;
    Dialog->Params = Params;
    Dialog->LocalDirectory = LocalDirectory;
    Dialog->RemoteDirectory = RemoteDirectory;
    Dialog->CopyParams = *CopyParams;
    Dialog->SaveSettings = SaveSettings;
    Dialog->SaveMode = SaveMode;
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
  FPresetsMenu = new TPopupMenu(this);
  InstallPathWordBreakProc(LocalDirectoryEdit);
  InstallPathWordBreakProc(RemoteDirectoryEdit);
  FSynchronizeBySizeCaption = SynchronizeBySizeCheck->Caption;
}
//---------------------------------------------------------------------------
__fastcall TFullSynchronizeDialog::~TFullSynchronizeDialog()
{
  delete FPresetsMenu;
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::UpdateControls()
{
  EnableControl(SynchronizeTimestampCheck, FLAGCLEAR(Options, fsoDisableTimestamp));
  if (SynchronizeTimestampCheck->Checked)
  {
    SynchronizeExistingOnlyCheck->Checked = true;
    SynchronizePreviewChangesCheck->Checked = false;
  }
  if (SynchronizeTimestampCheck->Checked)
  {
    SynchronizeDeleteCheck->Checked = false;
    SynchronizeByTimeCheck->Checked = true;
  }
  if (SynchronizeBothButton->Checked)
  {
    SynchronizeBySizeCheck->Checked = false;
  }
  EnableControl(SynchronizeDeleteCheck, !SynchronizeBothButton->Checked && 
    !SynchronizeTimestampCheck->Checked);
  EnableControl(SynchronizeExistingOnlyCheck, !SynchronizeTimestampCheck->Checked);
  EnableControl(SynchronizePreviewChangesCheck, !SynchronizeTimestampCheck->Checked);
  EnableControl(SynchronizeByTimeCheck, !SynchronizeBothButton->Checked && 
    !SynchronizeTimestampCheck->Checked);
  EnableControl(SynchronizeBySizeCheck, !SynchronizeBothButton->Checked && 
    !SynchronizeTimestampCheck->Checked);
  EnableControl(SynchronizeBySizeCheck, !SynchronizeBothButton->Checked);
  EnableControl(SynchronizeSelectedOnlyCheck, FLAGSET(FOptions, fsoAllowSelectedOnly));
  EnableControl(OkButton, !LocalDirectoryEdit->Text.IsEmpty() &&
    !RemoteDirectoryEdit->Text.IsEmpty());

  AnsiString InfoStr = FCopyParams.GetInfoStr("; ",
    FLAGMASK(SynchronizeTimestampCheck->Checked, TCopyParamType::cpiExcludeMaskOnly));
  CopyParamLabel->Caption = InfoStr;
  CopyParamLabel->Hint = InfoStr;
  CopyParamLabel->ShowHint =
    (CopyParamLabel->Canvas->TextWidth(InfoStr) > (CopyParamLabel->Width * 3 / 2));
  SynchronizeBySizeCheck->Caption = SynchronizeTimestampCheck->Checked ?
    LoadStr(SYNCHRONIZE_SAME_SIZE) : FSynchronizeBySizeCaption;
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
  LocalDirectoryEdit->Items = CustomWinConfiguration->History["LocalDirectory"];
  RemoteDirectoryEdit->Items = CustomWinConfiguration->History["RemoteDirectory"];
  bool Result = (ShowModal() == mrOk);
  if (Result)
  {
    LocalDirectoryEdit->SaveToHistory();
    CustomWinConfiguration->History["LocalDirectory"] = LocalDirectoryEdit->Items;
    RemoteDirectoryEdit->SaveToHistory();
    CustomWinConfiguration->History["RemoteDirectory"] = RemoteDirectoryEdit->Items;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::SetRemoteDirectory(const AnsiString value)
{
  RemoteDirectoryEdit->Text = value;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TFullSynchronizeDialog::GetRemoteDirectory()
{
  return RemoteDirectoryEdit->Text;
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::SetLocalDirectory(const AnsiString value)
{
  LocalDirectoryEdit->Text = value;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TFullSynchronizeDialog::GetLocalDirectory()
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
      assert(false);
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
    assert(SynchronizeBothButton->Checked);
    return smBoth;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::SetParams(int value)
{
  FParams = value & ~(spDelete | spExistingOnly |
    spPreviewChanges | spTimestamp | spNotByTime | spBySize | spSelectedOnly);
  SynchronizeDeleteCheck->Checked = FLAGSET(value, spDelete);
  SynchronizeExistingOnlyCheck->Checked = FLAGSET(value, spExistingOnly);
  SynchronizePreviewChangesCheck->Checked = FLAGSET(value, spPreviewChanges);
  SynchronizeSelectedOnlyCheck->Checked = FLAGSET(value, spSelectedOnly);
  SynchronizeTimestampCheck->Checked = FLAGSET(value, spTimestamp) &&
    FLAGCLEAR(Options, fsoDisableTimestamp);
  SynchronizeByTimeCheck->Checked = FLAGCLEAR(value, spNotByTime);
  SynchronizeBySizeCheck->Checked = FLAGSET(value, spBySize);
  UpdateControls();
}
//---------------------------------------------------------------------------
int __fastcall TFullSynchronizeDialog::GetParams()
{
  return FParams |
    FLAGMASK(SynchronizeDeleteCheck->Checked, spDelete) |
    FLAGMASK(SynchronizeExistingOnlyCheck->Checked, spExistingOnly) |
    FLAGMASK(SynchronizePreviewChangesCheck->Checked, spPreviewChanges) |
    FLAGMASK(SynchronizeSelectedOnlyCheck->Checked, spSelectedOnly) |
    FLAGMASK(SynchronizeTimestampCheck->Checked && FLAGCLEAR(Options, fsoDisableTimestamp),
      spTimestamp) |
    FLAGMASK(!SynchronizeByTimeCheck->Checked, spNotByTime) |
    FLAGMASK(SynchronizeBySizeCheck->Checked, spBySize);
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::LocalDirectoryBrowseButtonClick(
      TObject * /*Sender*/)
{
  AnsiString Directory = LocalDirectoryEdit->Text;
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
void __fastcall TFullSynchronizeDialog::SetOptions(int value)
{
  if (Options != value)
  {
    FOptions = value;
    if (FLAGSET(Options, fsoDisableTimestamp))
    {
      SynchronizeTimestampCheck->Checked = false;
    }
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::TransferSettingsButtonClick(
  TObject * /*Sender*/)
{
  if (FLAGCLEAR(FOptions, fsoDoNotUsePresets))
  {
    CopyParamListPopup(
      TransferSettingsButton->ClientToScreen(TPoint(0, TransferSettingsButton->Height)),
      FPresetsMenu, FCopyParams, FPreset, CopyParamClick, cplCustomize);
  }
  else
  {
    CopyParamGroupDblClick(NULL);
  }
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::CopyParamClick(TObject * Sender)
{
  assert(FLAGCLEAR(FOptions, fsoDoNotUsePresets));
  if (CopyParamListPopupClick(Sender, FCopyParams, FPreset))
  {
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::FormShow(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::FormCloseQuery(TObject * /*Sender*/,
  bool & CanClose)
{
  if ((ModalResult != mrCancel) &&
      SaveSettings && (FOrigMode != Mode) && !FSaveMode)
  {
    switch (MessageDialog(LoadStr(SAVE_SYNCHRONIZE_MODE),
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
void __fastcall TFullSynchronizeDialog::SynchronizeByTimeSizeCheckClick(
  TObject * Sender)
{
  if (!dynamic_cast<TCheckBox*>(Sender)->Checked)
  {
    (Sender == SynchronizeByTimeCheck ? SynchronizeBySizeCheck : SynchronizeByTimeCheck)->
      Checked = true;
  }
  UpdateControls();
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
    CopyParamListPopup(CopyParamGroup->ClientToScreen(MousePos), FPresetsMenu,
      FCopyParams, FPreset, CopyParamClick, cplCustomize | cplCustomizeDefault);
    Handled = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::CopyParamGroupDblClick(
  TObject * /*Sender*/)
{
  if (DoCopyParamCustomDialog(FCopyParams,
       (SynchronizeTimestampCheck->Checked ? cfAllowExcludeMaskOnly : -1)))
  {
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------

