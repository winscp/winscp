//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>

#include "WinInterface.h"
#include "Synchronize.h"
#include "VCLCommon.h"
#include "CopyParams.h"

#include <ScpMain.h>
#include <Configuration.h>
#include <TextsWin.h>
#include <HelpWin.h>
#include <CustomWinConfiguration.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "XPThemes"
#pragma link "HistoryComboBox"
#pragma link "GrayedCheckBox"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
const int WM_USER_STOP = WM_WINSCP_USER + 2;
//---------------------------------------------------------------------------
bool __fastcall DoSynchronizeDialog(TSynchronizeParamType & Params,
  const TCopyParamType * CopyParams, TSynchronizeStartStopEvent OnStartStop,
  bool & SaveSettings)
{
  bool Result;
  TSynchronizeDialog * Dialog = new TSynchronizeDialog(Application);
  try
  {
    Dialog->OnStartStop = OnStartStop;
    Dialog->Params = Params;
    Dialog->CopyParams = *CopyParams;
    Dialog->SaveSettings = SaveSettings;
    Result = Dialog->Execute();
    if (Result)
    {
      SaveSettings = Dialog->SaveSettings; 
      Params = Dialog->Params;
    }
  }
  __finally
  {
    delete Dialog;
  }

  return Result;
}
//---------------------------------------------------------------------------
__fastcall TSynchronizeDialog::TSynchronizeDialog(TComponent* Owner)
  : TForm(Owner)
{
  UseSystemSettings(this);
  FSynchronizing = false;
  FMinimizedByMe = false;
  FPresetsMenu = new TPopupMenu(this);

  InstallPathWordBreakProc(LocalDirectoryEdit);
  InstallPathWordBreakProc(RemoteDirectoryEdit);
}
//---------------------------------------------------------------------------
__fastcall TSynchronizeDialog::~TSynchronizeDialog()
{
  delete FPresetsMenu;
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::UpdateControls()
{
  EnableControl(StartButton, !LocalDirectoryEdit->Text.IsEmpty() &&
    !RemoteDirectoryEdit->Text.IsEmpty());
  TButton * OldButton = FSynchronizing ? StartButton : StopButton;
  TButton * NewButton = FSynchronizing ? StopButton : StartButton;
  if (!NewButton->Visible || OldButton->Visible)
  {
    NewButton->Visible = true;
    if (OldButton->Focused())
    {
      NewButton->SetFocus();
    }
    OldButton->Default = false;
    NewButton->Default = true;
    OldButton->Visible = false;
    // some of the above steps hides accelerators when start button is pressed with mouse
    ResetSystemSettings(this);
  }
  Caption = LoadStr(FSynchronizing ? SYNCHRONIZE_SYCHRONIZING : SYNCHRONIZE_TITLE);
  EnableControl(TransferSettingsButton, !FSynchronizing);
  CancelButton->Visible = !FSynchronizing;
  EnableControl(DirectoriesGroup, !FSynchronizing);
  EnableControl(OptionsGroup, !FSynchronizing);
  EnableControl(CopyParamGroup, !FSynchronizing);
  MinimizeButton->Visible = FSynchronizing;

  AnsiString InfoStr = CopyParams.GetInfoStr("; ");
  CopyParamLabel->Caption = InfoStr;
  CopyParamLabel->Hint = InfoStr;
  CopyParamLabel->ShowHint =
    (CopyParamLabel->Canvas->TextWidth(InfoStr) > (CopyParamLabel->Width * 3 / 2));
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::ControlChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
bool __fastcall TSynchronizeDialog::Execute()
{
  // at start assume that copy param is current preset
  FPreset = GUIConfiguration->CopyParamCurrent;

  LocalDirectoryEdit->Items = CustomWinConfiguration->History["LocalDirectory"];
  RemoteDirectoryEdit->Items = CustomWinConfiguration->History["RemoteDirectory"];
  ShowModal();

  return true;
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::SetParams(const TSynchronizeParamType& value)
{
  FParams = value;
  RemoteDirectoryEdit->Text = value.RemoteDirectory;
  LocalDirectoryEdit->Text = value.LocalDirectory;
  SynchronizeDeleteCheck->Checked = FLAGSET(value.Params, spDelete);
  SynchronizeExistingOnlyCheck->Checked = FLAGSET(value.Params, spExistingOnly);
  SynchronizeNoConfirmationCheck->Checked = FLAGSET(value.Params, spNoConfirmation);
  SynchronizeRecursiveCheck->Checked = FLAGSET(value.Options, soRecurse);
  SynchronizeSynchronizeCheck->State =
    FLAGSET(value.Options, soSynchronizeAsk) ? cbGrayed :
      (FLAGSET(value.Options, soSynchronize) ? cbChecked : cbUnchecked);
}
//---------------------------------------------------------------------------
TSynchronizeParamType __fastcall TSynchronizeDialog::GetParams()
{
  TSynchronizeParamType Result = FParams;
  Result.RemoteDirectory = RemoteDirectoryEdit->Text;
  Result.LocalDirectory = LocalDirectoryEdit->Text;
  Result.Params =
    (Result.Params & ~(spDelete | spExistingOnly | spNoConfirmation)) |
    FLAGMASK(SynchronizeDeleteCheck->Checked, spDelete) |
    FLAGMASK(SynchronizeExistingOnlyCheck->Checked, spExistingOnly) |
    FLAGMASK(SynchronizeNoConfirmationCheck->Checked, spNoConfirmation);
  Result.Options =
    (Result.Options & ~(soRecurse | soSynchronize | soSynchronizeAsk)) |
    FLAGMASK(SynchronizeRecursiveCheck->Checked, soRecurse) |
    FLAGMASK(SynchronizeSynchronizeCheck->State == cbChecked, soSynchronize) |
    FLAGMASK(SynchronizeSynchronizeCheck->State == cbGrayed, soSynchronizeAsk);
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::LocalDirectoryBrowseButtonClick(
      TObject * /*Sender*/)
{
  AnsiString Directory = LocalDirectoryEdit->Text;
  if (SelectDirectory(Directory, LoadStr(SELECT_LOCAL_DIRECTORY), false))
  {
    LocalDirectoryEdit->Text = Directory;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::TransferSettingsButtonClick(
  TObject * /*Sender*/)
{
  CopyParamListPopup(
    TransferSettingsButton->ClientToScreen(TPoint(0, TransferSettingsButton->Height)),
    FPresetsMenu, FCopyParams, FPreset, CopyParamClick, cplCustomize);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::DoStartStop(bool Start, bool Synchronize)
{
  if (FOnStartStop)
  {
    TSynchronizeParamType SParams = GetParams();
    SParams.Options =
      (SParams.Options & ~(soSynchronize | soSynchronizeAsk)) |
      FLAGMASK(Synchronize, soSynchronize);
    FOnStartStop(this, Start, SParams, CopyParams, DoAbort, NULL);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::Dispatch(void * Message)
{
  assert(Message);
  if ((reinterpret_cast<TMessage *>(Message)->Msg == WM_USER_STOP) && FAbort)
  {
    if (FSynchronizing)
    {
      Stop();
    }
    if (FClose)
    {
      FClose = false;
      ModalResult = mrCancel;
    }
  }
  else
  {
    TForm::Dispatch(Message);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::DoAbort(TObject * /*Sender*/, bool Close)
{
  FAbort = true;
  FClose = Close;
  PostMessage(Handle, WM_USER_STOP, 0, 0);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::StartButtonClick(TObject * /*Sender*/)
{
  bool Synchronize;
  bool Continue = true;
  if (SynchronizeSynchronizeCheck->State == cbGrayed)
  {
    TMessageParams Params(mpNeverAskAgainCheck);
    switch (MoreMessageDialog(LoadStr(SYNCHRONISE_BEFORE_KEEPUPTODATE),
        NULL, qtConfirmation, qaYes | qaNo | qaCancel, HELP_KEEPUPTODATE_SYNCHRONIZE,
        &Params))
    {
      case qaNeverAskAgain:
        SynchronizeSynchronizeCheck->State = cbChecked;
        // fall thru
        break;

      case qaYes:
        Synchronize = true;
        break;

      case qaNo:
        Synchronize = false;
        break;

      default:
      case qaCancel:
        Continue = false;
        break;
    };
  }
  else
  {
    Synchronize = SynchronizeSynchronizeCheck->Checked;
  }

  if (Continue)
  {
    assert(!FSynchronizing);

    LocalDirectoryEdit->SaveToHistory();
    CustomWinConfiguration->History["LocalDirectory"] = LocalDirectoryEdit->Items;
    RemoteDirectoryEdit->SaveToHistory();
    CustomWinConfiguration->History["RemoteDirectory"] = RemoteDirectoryEdit->Items;

    FSynchronizing = true;
    try
    {
      UpdateControls();

      FAbort = false;
      DoStartStop(true, Synchronize);
    }
    catch(...)
    {
      FSynchronizing = false;
      UpdateControls();
      throw;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::StopButtonClick(TObject * /*Sender*/)
{
  Stop();
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::Stop()
{
  FSynchronizing = false;
  DoStartStop(false, false);
  UpdateControls();
  if (IsIconic(Application->Handle) && FMinimizedByMe)
  {
    FMinimizedByMe = false;
    Application->Restore();
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::MinimizeButtonClick(TObject * /*Sender*/)
{
  Application->Minimize();
  FMinimizedByMe = true;
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::SetSaveSettings(bool value)
{
  SaveSettingsCheck->Checked = value;
}
//---------------------------------------------------------------------------
bool __fastcall TSynchronizeDialog::GetSaveSettings()
{
  return SaveSettingsCheck->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::FormShow(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::FormCloseQuery(TObject * /*Sender*/,
  bool & /*CanClose*/)
{
  if (FSynchronizing)
  {
    Stop();
  }
}
//---------------------------------------------------------------------------
TCopyParamType __fastcall TSynchronizeDialog::GetCopyParams()
{
  TCopyParamType Result = FCopyParams;
  Result.PreserveTime = true;
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::SetCopyParams(const TCopyParamType & value)
{
  FCopyParams = value;
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::CopyParamClick(TObject * Sender)
{
  if (CopyParamListPopupClick(Sender, FCopyParams, FPreset,
        cfAllowTransferMode | cfAllowExcludeMask | cfAllowClearArchive | cfDisablePreserveTime))
  {
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::CopyParamGroupContextPopup(
  TObject * /*Sender*/, TPoint & MousePos, bool & Handled)
{
  CopyParamListPopup(CopyParamGroup->ClientToScreen(MousePos), FPresetsMenu,
    FCopyParams, FPreset, CopyParamClick, cplCustomize | cplCustomizeDefault);
  Handled = true;
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::CopyParamGroupDblClick(TObject * /*Sender*/)
{
  if (DoCopyParamCustomDialog(FCopyParams, cfDisablePreserveTime))
  {
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------

