//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>

#include "Synchronize.h"
#include "VCLCommon.h"

#include <ScpMain.h>
#include <Configuration.h>
#include <TextsWin.h>
#include <CustomWinConfiguration.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "XPGroupBox"
#pragma link "HistoryComboBox"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
const int WM_USER_STOP = WM_USER + 100;
//---------------------------------------------------------------------------
bool __fastcall DoSynchronizeDialog(TSynchronizeParamType & Params,
  TSynchronizeStartStopEvent OnStartStop, bool & SaveSettings)
{
  bool Result;
  TSynchronizeDialog * Dialog = new TSynchronizeDialog(Application);
  try
  {
    Dialog->OnStartStop = OnStartStop;
    Dialog->Params = Params;
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
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::UpdateControls()
{
  EnableControl(StartButton, !LocalDirectoryEdit->Text.IsEmpty() &&
    !RemoteDirectoryEdit->Text.IsEmpty());
  StartButton->Visible = !FSynchronizing;
  StartButton->Default = StartButton->Visible;
  StopButton->Visible = FSynchronizing;
  StopButton->Default = StopButton->Visible;
  Caption = LoadStr(FSynchronizing ? SYNCHRONIZE_SYCHRONIZING : SYNCHRONIZE_TITLE);
  EnableControl(TransferPreferencesButton, !FSynchronizing);
  EnableControl(CancelButton, !FSynchronizing);
  EnableControl(DirectoriesGroup, !FSynchronizing);
  EnableControl(OptionsGroup, !FSynchronizing);
  EnableControl(MinimizeButton, FSynchronizing);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::ControlChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
bool __fastcall TSynchronizeDialog::Execute()
{
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
  SynchronizeRecursiveCheck->Checked = value.Recurse;
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
  Result.Recurse = SynchronizeRecursiveCheck->Checked;
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
void __fastcall TSynchronizeDialog::DirectoryEditKeyDown(
  TObject * Sender, WORD & Key, TShiftState Shift)
{
  PathComboBoxKeyDown(dynamic_cast<TCustomComboBox*>(Sender), Key, Shift,
    (Sender == RemoteDirectoryEdit));
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::TransferPreferencesButtonClick(
  TObject * /*Sender*/)
{
  DoPreferencesDialog(pmTransfer);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::DoStartStop(bool Start)
{
  if (FOnStartStop)
  {
    FOnStartStop(this, Start, GetParams(), DoAbort, NULL);
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
    DoStartStop(true);
  }
  catch(...)
  {
    FSynchronizing = false;
    UpdateControls();
    throw;
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
  DoStartStop(false);
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

