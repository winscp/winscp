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
#include <WinConfiguration.h>
#include <TerminalManager.h>
#include <StrUtils.hpp>
#include <Tools.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "HistoryComboBox"
#pragma link "GrayedCheckBox"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
const int WM_USER_STOP = WM_WINSCP_USER + 2;
//---------------------------------------------------------------------------
bool __fastcall DoSynchronizeDialog(TSynchronizeParamType & Params,
  const TCopyParamType * CopyParams, TSynchronizeStartStopEvent OnStartStop,
  bool & SaveSettings, int Options, int CopyParamAttrs,
  TGetSynchronizeOptionsEvent OnGetOptions,
  TSynchronizeSessionLog OnSynchronizeSessionLog,
  TFeedSynchronizeError & OnFeedSynchronizeError,
  TNotifyEvent & OnSynchronizeAbort,
  TSynchronizeInNewWindow OnSynchronizeInNewWindow,
  int AutoSubmit)
{
  bool Result;
  TSynchronizeDialog * Dialog = SafeFormCreate<TSynchronizeDialog>(Application);

  Dialog->Init(
    OnStartStop, OnGetOptions, OnSynchronizeSessionLog, OnFeedSynchronizeError, OnSynchronizeAbort,
    OnSynchronizeInNewWindow, AutoSubmit);

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
    OnSynchronizeAbort = NULL;
  }

  return Result;
}
//---------------------------------------------------------------------------
const TSynchronizeDialog::MaxLogItems = 1000;
//---------------------------------------------------------------------------
struct TLogItemData
{
  TSynchronizeLogEntry Entry;
  UnicodeString Message;
  std::unique_ptr<TStrings> MoreMessages;
  TQueryType Type;
  UnicodeString HelpKeyword;
};
//---------------------------------------------------------------------------
__fastcall TSynchronizeDialog::TSynchronizeDialog(TComponent * Owner)
  : TForm(Owner)
{
  UseSystemSettings(this);
  FOptions = 0;
  FSynchronizing = false;
  FMinimizedByMe = false;
  FPresetsMenu = new TPopupMenu(this);
  FSynchronizeOptions = NULL;

  HotTrackLabel(CopyParamLabel);
  LoadDialogImage(Image, L"Keep remote directory up to date");

  SetGlobalMinimizeHandler(this, GlobalMinimize);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::Init(TSynchronizeStartStopEvent OnStartStop,
  TGetSynchronizeOptionsEvent OnGetOptions,
  TSynchronizeSessionLog OnSynchronizeSessionLog,
  TFeedSynchronizeError & OnFeedSynchronizeError,
  TNotifyEvent & OnSynchronizeAbort,
  TSynchronizeInNewWindow OnSynchronizeInNewWindow,
  int AutoSubmit)
{
  FOnStartStop = OnStartStop;
  FOnGetOptions = OnGetOptions;
  FOnSynchronizeSessionLog = OnSynchronizeSessionLog;
  FOnFeedSynchronizeError = &OnFeedSynchronizeError;
  OnSynchronizeAbort = &SynchronizeAbort;
  DebugAssert(OnSynchronizeInNewWindow != NULL);
  FOnSynchronizeInNewWindow = OnSynchronizeInNewWindow;
  if (AutoSubmit == 0)
  {
    FStartImmediately = true;
  }
  else
  {
    FStartImmediately = false;

    if (AutoSubmit > 0)
    {
      InitiateDialogTimeout(this, AutoSubmit * MSecsPerSec, StartButton);
    }
  }
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

  ClearGlobalMinimizeHandler(GlobalMinimize);

  delete FSynchronizeOptions;
  delete FPresetsMenu;
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::FeedSynchronizeError(
  const UnicodeString & Message, TStrings * MoreMessages, TQueryType Type,
  const UnicodeString & HelpKeyword)
{
  DoLogInternal(slContinuedError, Message, MoreMessages, Type, HelpKeyword);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::SynchronizeAbort(TObject *)
{
  Abort(true);
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
  TTerminalManager * Manager = TTerminalManager::Instance();
  UnicodeString Title = LoadStr(FSynchronizing ? SYNCHRONIZE_SYCHRONIZING : SYNCHRONIZE_TITLE);
  Caption = Manager->FormatFormCaptionWithSession(this, Title);
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
  SetLabelHintPopup(CopyParamLabel, InfoStr);

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

  // When minimizing to tray globally, no point showing special "minimize to tray" command
  MinimizeButton->Style =
    !WinConfiguration->MinimizeToTray ? TCustomButton::bsSplitButton : TCustomButton::bsPushButton;

  StartButton->Style = AllowStartInNewWindow() ? TCustomButton::bsSplitButton : TCustomButton::bsPushButton;
  StartInNewWindowItem->Enabled = CanStartInNewWindow();
}
//---------------------------------------------------------------------------
bool __fastcall TSynchronizeDialog::AllowStartInNewWindow()
{
  return !IsMainFormLike(this);
}
//---------------------------------------------------------------------------
bool __fastcall TSynchronizeDialog::CanStartInNewWindow()
{
  return
    AllowStartInNewWindow() &&
    (!SynchronizeSelectedOnlyCheck->Enabled || !SynchronizeSelectedOnlyCheck->Checked);
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
  SynchronizeDeleteCheck->Checked = FLAGSET(value.Params, TTerminal::spDelete);
  SynchronizeExistingOnlyCheck->Checked = FLAGSET(value.Params, TTerminal::spExistingOnly);
  SynchronizeSelectedOnlyCheck->Checked = FLAGSET(value.Params, TTerminal::spSelectedOnly);
  SynchronizeRecursiveCheck->Checked = FLAGSET(value.Options, soRecurse);
  SynchronizeSynchronizeCheck->State =
    FLAGSET(value.Options, soSynchronizeAsk) ? cbGrayed :
      (FLAGSET(value.Options, soSynchronize) ? cbChecked : cbUnchecked);
  ContinueOnErrorCheck->Checked = FLAGSET(value.Options, soContinueOnError);
}
//---------------------------------------------------------------------------
TSynchronizeParamType __fastcall TSynchronizeDialog::GetParams()
{
  TSynchronizeParamType Result = FParams;
  Result.RemoteDirectory = RemoteDirectoryEdit->Text;
  Result.LocalDirectory = LocalDirectoryEdit->Text;
  Result.Params =
    (Result.Params & ~(TTerminal::spDelete | TTerminal::spExistingOnly | TTerminal::spSelectedOnly | TTerminal::spTimestamp)) |
    FLAGMASK(SynchronizeDeleteCheck->Checked, TTerminal::spDelete) |
    FLAGMASK(SynchronizeExistingOnlyCheck->Checked, TTerminal::spExistingOnly) |
    FLAGMASK(SynchronizeSelectedOnlyCheck->Checked, TTerminal::spSelectedOnly);
  Result.Options =
    (Result.Options & ~(soRecurse | soSynchronize | soSynchronizeAsk | soContinueOnError)) |
    FLAGMASK(SynchronizeRecursiveCheck->Checked, soRecurse) |
    FLAGMASK(SynchronizeSynchronizeCheck->State == cbChecked, soSynchronize) |
    FLAGMASK(SynchronizeSynchronizeCheck->State == cbGrayed, soSynchronizeAsk) |
    FLAGMASK(ContinueOnErrorCheck->Checked, soContinueOnError);
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
void __fastcall TSynchronizeDialog::CopyParamListPopup(TRect R, int AdditionalOptions)
{
  // We pass in FCopyParams, although it may not be the exact copy param
  // that will be used (because of PreserveTime). The reason is to
  // display checkbox next to user-selected preset
  ::CopyParamListPopup(
    R, FPresetsMenu, FCopyParams, FPreset, CopyParamClick,
    AdditionalOptions, ActualCopyParamAttrs());
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::TransferSettingsButtonClick(
  TObject * /*Sender*/)
{
  CopyParamGroupClick(NULL);
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
      DebugAssert(*FOnFeedSynchronizeError == NULL);
      *FOnFeedSynchronizeError =
        (FLAGSET(SParams.Options, soContinueOnError) ? &FeedSynchronizeError : TFeedSynchronizeError(NULL));
      delete FSynchronizeOptions;
      FSynchronizeOptions = new TSynchronizeOptions;
      FOnGetOptions(SParams.Params, *FSynchronizeOptions);
    }
    else
    {
      *FOnFeedSynchronizeError = NULL;
    }
    FOnStartStop(this, Start, SParams, CopyParams, FSynchronizeOptions, DoAbort,
      NULL, DoLog);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::Dispatch(void * AMessage)
{
  DebugAssert(AMessage != NULL);
  TMessage & Message = *reinterpret_cast<TMessage *>(AMessage);
  if ((Message.Msg == WM_USER_STOP) && FAbort)
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
  else if (Message.Msg == WM_MANAGES_CAPTION)
  {
    // caption managed in UpdateControls()
    Message.Result = 1;
  }
  else
  {
    TForm::Dispatch(AMessage);
  }
}
//---------------------------------------------------------------------------
void TSynchronizeDialog::Abort(bool Close)
{
  FAbort = true;
  FClose = Close;
  PostMessage(Handle, WM_USER_STOP, 0, 0);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::DoAbort(TObject * /*Sender*/, bool Close)
{
  Abort(Close);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::DoLogInternal(
  TSynchronizeLogEntry Entry, const UnicodeString & Message,
  TStrings * MoreMessages, TQueryType Type, const UnicodeString & HelpKeyword)
{
  LogView->Items->BeginUpdate();
  try
  {
    TListItem * Item = LogView->Items->Add();

    TLogItemData * LogItemData = new TLogItemData();
    Item->Data = LogItemData;
    LogItemData->Entry = Entry;
    LogItemData->Message = Message;
    if (MoreMessages != NULL)
    {
      LogItemData->MoreMessages.reset(new TStringList());
      LogItemData->MoreMessages->Assign(MoreMessages);
    }
    LogItemData->Type = Type;
    LogItemData->HelpKeyword = HelpKeyword;

    Item->Caption = Now().TimeString();

    UnicodeString UnformattedMessage = UnformatMessage(Message);
    UnformattedMessage = ReplaceStr(UnformattedMessage, L"\r", L"");
    UnformattedMessage = ReplaceStr(UnformattedMessage, L"\n", L" ");

    Item->SubItems->Add(UnformattedMessage);
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

  FOnSynchronizeSessionLog(Message);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::DoLog(TSynchronizeController * /*Controller*/,
  TSynchronizeLogEntry Entry, const UnicodeString Message)
{
  DoLogInternal(Entry, Message,
    // these are unused (as Entry is not slContinuedError here)
    NULL, qtInformation, UnicodeString());
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::StartButtonClick(TObject * /*Sender*/)
{
  if (OpenInNewWindow())
  {
    if (CanStartInNewWindow())
    {
      StartInNewWindow();
    }
    else
    {
      Beep();
    }
  }
  else
  {
    Start();
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::Start()
{
  bool Synchronize;
  bool Continue = true;
  if (SynchronizeSynchronizeCheck->State == cbGrayed)
  {
    TMessageParams Params(mpNeverAskAgainCheck);
    switch (MoreMessageDialog(LoadStr(SYNCHRONISE_BEFORE_KEEPUPTODATE2),
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
    DebugAssert(!FSynchronizing);

    SaveHistory();

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
void __fastcall TSynchronizeDialog::SaveHistory()
{
  LocalDirectoryEdit->SaveToHistory();
  CustomWinConfiguration->History[L"LocalDirectory"] = LocalDirectoryEdit->Items;
  RemoteDirectoryEdit->SaveToHistory();
  CustomWinConfiguration->History[L"RemoteDirectory"] = RemoteDirectoryEdit->Items;
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
    ApplicationRestore();
    Application->BringToFront();
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::Minimize(TObject * Sender)
{
  CallGlobalMinimizeHandler(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::MinimizeButtonClick(TObject * Sender)
{
  Minimize(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::GlobalMinimize(TObject * /*Sender*/)
{
  ApplicationMinimize();
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
    if (FStartImmediately)
    {
      // if starting get cancelled (from SYNCHRONISE_BEFORE_KEEPUPTODATE2 prompt),
      // and OnShow gets called again (FSynchronizing is false),
      // we do not want to try to start again
      FStartImmediately = false;
      StartButtonClick(NULL);
    }
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
  Result.NewerOnly = false;
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
  return FCopyParamAttrs | cpaNoPreserveTime | cpaNoNewerOnly;
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::CopyParamClick(TObject * Sender)
{
  DebugAssert(FLAGCLEAR(FOptions, soDoNotUsePresets));
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
void __fastcall TSynchronizeDialog::CopyParamGroupContextPopup(
  TObject * /*Sender*/, TPoint & MousePos, bool & Handled)
{
  if (FLAGCLEAR(FOptions, soDoNotUsePresets))
  {
    CopyParamListPopup(CalculatePopupRect(CopyParamGroup, MousePos),
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
  TInstantOperationVisualizer Visualizer;
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
  CopyParamListPopup(CalculatePopupRect(TransferSettingsButton), cplCustomizeDefault);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::LogViewCustomDrawItem(TCustomListView * Sender,
  TListItem * Item, TCustomDrawState /*State*/, bool & /*DefaultDraw*/)
{
  TLogItemData * LogItemData = GetLogItemData(Item);
  if (LogItemData->Entry == slContinuedError)
  {
    Sender->Canvas->Font->Color = clRed;
  }
}
//---------------------------------------------------------------------------
TLogItemData * __fastcall TSynchronizeDialog::GetLogItemData(TListItem * Item)
{
  return reinterpret_cast<TLogItemData *>(Item->Data);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::LogViewDeletion(TObject * /*Sender*/, TListItem * Item)
{
  delete GetLogItemData(Item);
  Item->Data = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::LogViewDblClick(TObject * /*Sender*/)
{
  if (LogView->ItemFocused != NULL)
  {
    TLogItemData * LogItemData = GetLogItemData(LogView->ItemFocused);
    if (LogItemData->Entry == slContinuedError)
    {
      MoreMessageDialog(
        LogItemData->Message, LogItemData->MoreMessages.get(), LogItemData->Type,
        qaOK, LogItemData->HelpKeyword);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::Minimize1Click(TObject * Sender)
{
  Minimize(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::MinimizetoTray1Click(TObject * Sender)
{
  bool MinimizeToTrayPrev = WinConfiguration->MinimizeToTray;
  DebugAssert(!MinimizeToTrayPrev);
  WinConfiguration->MinimizeToTray = true;
  try
  {
    Minimize(Sender);
  }
  __finally
  {
    WinConfiguration->MinimizeToTray = MinimizeToTrayPrev;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::MinimizeButtonDropDownClick(TObject * /*Sender*/)
{
  MenuPopup(MinimizeMenu, MinimizeButton);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::StartInNewWindowItemClick(TObject * /*Sender*/)
{
  StartInNewWindow();
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::StartInNewWindow()
{
  SaveHistory();
  TSynchronizeParamType AParams = Params;
  TCopyParamType ACopyParams = CopyParams;
  FOnSynchronizeInNewWindow(AParams, &ACopyParams);
  Close();
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeDialog::StartButtonDropDownClick(TObject * /*Sender*/)
{
  MenuPopup(StartMenu, StartButton);
}
//---------------------------------------------------------------------------
