//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>

#include "WinInterface.h"
#include "Synchronize.h"
#include "VCLCommon.h"
#include "CopyParams.h"
#include "Terminal.h"
#include "GUITools.h"

#include <CoreMain.h>
#include <Configuration.h>
#include <TextsWin.h>
#include <HelpWin.h>
#include <CustomWinConfiguration.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "HistoryComboBox"
#pragma link "GrayedCheckBox"
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------------
const int WM_USER_STOP = WM_WINSCP_USER + 2;
//---------------------------------------------------------------------------
bool __fastcall DoSynchronizeDialog(TSynchronizeParamType & Params,
  const TCopyParamType * CopyParams, TSynchronizeStartStopEvent OnStartStop,
  bool & SaveSettings, int Options, int CopyParamAttrs,
  TGetSynchronizeOptionsEvent OnGetOptions, bool Start)
{
  bool Result;
  TSynchronizeDialog * Dialog = new TSynchronizeDialog(Application,
    OnStartStop, OnGetOptions, Start);
  try
  {
    Dialog->Options = Options;
    Dialog->CopyParamAttrs = CopyParamAttrs;
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
const TSynchronizeDialog::MaxLogItems = 1000;
//---------------------------------------------------------------------------
__fastcall TSynchronizeDialog::TSynchronizeDialog(TComponent * Owner,
  TSynchronizeStartStopEvent OnStartStop, TGetSynchronizeOptionsEvent OnGetOptions,
  bool StartImmediatelly)
  : TForm(Owner)
{
  UseSystemSettings(this);
  FOptions = 0;
  FSynchronizing = false;
  FMinimizedByMe = false;
  FPresetsMenu = new TPopupMenu(this);
  FOnStartStop = OnStartStop;
  FOnGetOptions = OnGetOptions;
  FSynchronizeOptions = NULL;
  FStartImmediatelly = StartImmediatelly;

  HotTrackLabel(CopyParamLabel);

  if (!IsGlobalMinimizeHandler())
  {
    SetGlobalMinimizeHandler(GlobalMinimize);
  };
}
//---------------------------------------------------------------------------
__fastcall TSynchronizeDialog::~TSynchronizeDialog()
{
  // if application is closing OnCloseQuery might not get called
  // (this particularly happens if last terminal is disconnected while dialog is
  // open)
  if (FSynchronizing)
  {
    OnlyStop();
  }

  if (GetGlobalMinimizeHandler() == GlobalMinimize)
  {
    SetGlobalMinimizeHandler(NULL);
  }

  delete FSynchronizeOptions;
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
  CancelButton->Visible = !FSynchronizing || FLAGSET(FOptions, soNoMinimize);
  EnableControl(CancelButton, !FSynchronizing);
  EnableControl(DirectoriesGroup, !FSynchronizing);
  EnableControl(OptionsGroup, !FSynchronizing);
  EnableControl(CopyParamGroup, !FSynchronizing);
  MinimizeButton->Visible = FSynchronizing && FLAGCLEAR(FOptions, soNoMinimize);
  EnableControl(SynchronizeSelectedOnlyCheck,
    OptionsGroup->Enabled && FLAGSET(FOptions, soAllowSelectedOnly));

  UnicodeString InfoStr = CopyParams.GetInfoStr(L"; ", ActualCopyParamAttrs());
  CopyParamLabel->Caption = InfoStr;
  CopyParamLabel->Hint = InfoStr;
  CopyParamLabel->ShowHint =
    (CopyParamLabel->Canvas->TextWidth(InfoStr) > (CopyParamLabel->Width * 3 / 2));

  TransferSettingsButton->Style =
    FLAGCLEAR(Options, soDoNotUsePresets) ?
      TCustomButton::bsSplitButton : TCustomButton::bsPushButton;

  if (LogPanel->Visible != FSynchronizing)
  {
    if (FSynchronizing)
    {
      LogPanel->Visible = true;
      ClientHeight = ClientHeight + LogPanel->Height;
    }
    else
    {
      ClientHeight = ClientHeight - LogPanel->Height;
      LogPanel->Visible = false;
    }
  }
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

  LocalDirectoryEdit->Items = CustomWinConfiguration->History[L"LocalDirectory"];
  RemoteDirectoryEdit->Items = CustomWinConfiguration->History[L"RemoteDirectory"];
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
  SynchronizeSelectedOnlyCheck->Checked = FLAGSET(value.Params, spSelectedOnly);
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
    (Result.Params & ~(spDelete | spExistingOnly | spSelectedOnly | spTimestamp)) |
    FLAGMASK(SynchronizeDeleteCheck->Checked, spDelete) |
    FLAGMASK(SynchronizeExistingOnlyCheck->Checked, spExistingOnly) |
    FLAGMASK(SynchronizeSelectedOnlyCheck->Checked, spSelectedOnly);
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
  UnicodeString Directory = LocalDirectoryEdit->Text;
  if (SelectDirectory(Directory, LoadStr(SELECT_LOCAL_DIRECTORY), false))
  {
    LocalDirectoryEdit->Text = Directory;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::SetOptions(int value)
{
  if (Options != value)
  {
    FOptions = value;
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::CopyParamListPopup(TPoint P, int AdditionalOptions)
{
  // We pass in FCopyParams, although it may not be the exact copy param
  // that will be used (because of PreserveTime). The reason is to
  // display checkbox next to user-selected preset
  ::CopyParamListPopup(
    P, FPresetsMenu, FCopyParams, FPreset, CopyParamClick, cplCustomize | AdditionalOptions);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::TransferSettingsButtonClick(
  TObject * /*Sender*/)
{
  if (FLAGCLEAR(FOptions, soDoNotUsePresets) && !SupportsSplitButton())
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
void __fastcall TSynchronizeDialog::DoStartStop(bool Start, bool Synchronize)
{
  if (FOnStartStop)
  {
    TSynchronizeParamType SParams = GetParams();
    SParams.Options =
      (SParams.Options & ~(soSynchronize | soSynchronizeAsk)) |
      FLAGMASK(Synchronize, soSynchronize);
    if (Start)
    {
      delete FSynchronizeOptions;
      FSynchronizeOptions = new TSynchronizeOptions;
      FOnGetOptions(SParams.Params, *FSynchronizeOptions);
    }
    FOnStartStop(this, Start, SParams, CopyParams, FSynchronizeOptions, DoAbort,
      NULL, DoLog);
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
void __fastcall TSynchronizeDialog::DoLog(TSynchronizeController * /*Controller*/,
  TSynchronizeLogEntry Entry, const UnicodeString Message)
{
  LogView->Items->BeginUpdate();
  try
  {
    TListItem * Item = LogView->Items->Add();
    Item->Caption = Now().TimeString();
    Item->SubItems->Add(Message);
    Item->MakeVisible(false);
    while (LogView->Items->Count > MaxLogItems)
    {
      LogView->Items->Delete(0);
    }
  }
  __finally
  {
    LogView->Items->EndUpdate();
    if (Entry == slScan)
    {
      // redraw log before the scanning block update
      LogView->Repaint();
    }
  }
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
    CustomWinConfiguration->History[L"LocalDirectory"] = LocalDirectoryEdit->Items;
    RemoteDirectoryEdit->SaveToHistory();
    CustomWinConfiguration->History[L"RemoteDirectory"] = RemoteDirectoryEdit->Items;

    FSynchronizing = true;
    try
    {
      UpdateControls();
      Repaint();

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
void __fastcall TSynchronizeDialog::OnlyStop()
{
  FSynchronizing = false;
  DoStartStop(false, false);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::Stop()
{
  OnlyStop();
  UpdateControls();
  Repaint();
  if (IsApplicationMinimized() && FMinimizedByMe)
  {
    FMinimizedByMe = false;
    Application->Restore();
    Application->BringToFront();
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::MinimizeButtonClick(TObject * /*Sender*/)
{
  MinimizeApp();
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::GlobalMinimize(TObject * /*Sender*/)
{
  MinimizeApp();
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::MinimizeApp()
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
  InstallPathWordBreakProc(LocalDirectoryEdit);
  InstallPathWordBreakProc(RemoteDirectoryEdit);

  // OnShow gets called more than once sometimes
  if (!FSynchronizing)
  {
    ClearLog();
    UpdateControls();
    if (FStartImmediatelly)
    {
      StartButtonClick(NULL);
    }
  }
  else
  {
    assert(FStartImmediatelly);
  }
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
int __fastcall TSynchronizeDialog::ActualCopyParamAttrs()
{
  return FCopyParamAttrs | cpaNoPreserveTime;
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::CopyParamClick(TObject * Sender)
{
  assert(FLAGCLEAR(FOptions, soDoNotUsePresets));
  // PreserveTime is forced for some settings, but avoid hard-setting it until
  // user really confirms it on custom dialog
  TCopyParamType ACopyParams = CopyParams;
  if (CopyParamListPopupClick(Sender, ACopyParams, FPreset,
        ActualCopyParamAttrs()))
  {
    FCopyParams = ACopyParams;
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::CopyParamGroupContextPopup(
  TObject * /*Sender*/, TPoint & MousePos, bool & Handled)
{
  if (FLAGCLEAR(FOptions, soDoNotUsePresets))
  {
    CopyParamListPopup(CopyParamGroup->ClientToScreen(MousePos),
      cplCustomizeDefault);
    Handled = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::CopyParamGroupClick(TObject * /*Sender*/)
{
  // PreserveTime is forced for some settings, but avoid hard-setting it until
  // user really confirms it on cutom dialog
  TCopyParamType ACopyParams = CopyParams;
  if (DoCopyParamCustomDialog(ACopyParams, ActualCopyParamAttrs()))
  {
    FCopyParams = ACopyParams;
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::ClearLog()
{
  // TListItems::Clear() does nothing without allocated handle
  LogView->HandleNeeded();
  LogView->Items->Clear();
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::CopyLog()
{
  UnicodeString Content;
  for (int i = 0; i < LogView->Items->Count; i++)
  {
    TListItem * Item = LogView->Items->Item[i];
    Content += Item->Caption + L"\t" + Item->SubItems->Strings[0] + L"\r\n";
  }
  CopyToClipboard(Content);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::LogViewKeyDown(TObject * /*Sender*/,
  WORD & Key, TShiftState Shift)
{
  if (Key == VK_DELETE)
  {
    ClearLog();
    Key = 0;
  }
  else if ((Key == L'C') && Shift.Contains(ssCtrl) && (LogView->Items->Count > 0))
  {
    CopyLog();
    Key = 0;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::FormKeyDown(TObject * /*Sender*/, WORD & Key,
  TShiftState /*Shift*/)
{
  if ((Key == VK_ESCAPE) && FSynchronizing)
  {
    Stop();
    Key = 0;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::TransferSettingsButtonDropDownClick(TObject * /*Sender*/)
{
  CopyParamListPopup(
    TransferSettingsButton->ClientToScreen(TPoint(0, TransferSettingsButton->Height)),
    cplCustomizeDefault);
}
//---------------------------------------------------------------------------
