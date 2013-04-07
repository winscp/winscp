//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <StrUtils.hpp>
#include <Common.h>
#include <math.h>

#include "Preferences.h"

#include <CoreMain.h>
#include <Terminal.h>
#include <Bookmarks.h>

#include "VCLCommon.h"
#include "GUITools.h"
#include "Tools.h"
#include "TextsWin.h"
#include "HelpWin.h"
#include "WinInterface.h"
#include "WinConfiguration.h"
#include "Setup.h"
//---------------------------------------------------------------------
#pragma link "GeneralSettings"
#pragma link "LogSettings"
#pragma link "CopyParams"
#pragma link "UpDownEdit"
#pragma link "ComboEdit"
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------
bool __fastcall DoPreferencesDialog(TPreferencesMode APreferencesMode,
  TPreferencesDialogData * DialogData)
{
  bool Result;
  TPreferencesDialog * PreferencesDialog = new TPreferencesDialog(GetFormOwner());
  try
  {
    PreferencesDialog->PreferencesMode = APreferencesMode;
    Result = PreferencesDialog->Execute(DialogData);
  }
  __finally
  {
    delete PreferencesDialog;
  }
  return Result;
}
//---------------------------------------------------------------------
__fastcall TPreferencesDialog::TPreferencesDialog(TComponent* AOwner)
  : TForm(AOwner)
{
  SetCorrectFormParent(this);

  FNoUpdate = 0;
  FPreferencesMode = ::pmDefault;
  FEditorFont = new TFont();
  FEditorFont->Color = clWindowText;
  // color tends to reset in object inspector
  EditorFontLabel->Color = clWindow;
  // currently useless
  FAfterFilenameEditDialog = false;
  FCustomCommandList = new TCustomCommandList();
  FCustomCommandChanging = false;
  FListViewDragDest = -1;
  FCopyParamList = new TCopyParamList();
  FEditorList = new TEditorList();
  UseSystemSettings(this);

  FCustomCommandsScrollOnDragOver = new TListViewScrollOnDragOver(CustomCommandsView, true);
  FCopyParamScrollOnDragOver = new TListViewScrollOnDragOver(CopyParamListView, true);
  FEditorScrollOnDragOver = new TListViewScrollOnDragOver(EditorListView2, true);

  ComboAutoSwitchInitialize(UpdatesBetaVersionsCombo);
  EnableControl(UpdatesBetaVersionsCombo, !WinConfiguration->IsBeta);
  EnableControl(UpdatesBetaVersionsLabel, UpdatesBetaVersionsCombo->Enabled);

  LoggingFrame->Init();
  HintLabel(ShellIconsText);
  HotTrackLabel(CopyParamLabel);

  EditorEncodingCombo->Items->Add(DefaultEncodingName());
  EditorEncodingCombo->Items->Add(LoadStr(UTF8_NAME));

  std::auto_ptr<TStrings> Workspaces(StoredSessions->GetWorkspaces());
  AutoWorkspaceCombo->Items->AddStrings(Workspaces.get());
  AutoSaveWorkspacePasswordsCheck->Caption = LoadStr(SAVE_WORKSPACE_PASSWORDS);
}
//---------------------------------------------------------------------------
__fastcall TPreferencesDialog::~TPreferencesDialog()
{
  SAFE_DESTROY(FEditorScrollOnDragOver);
  SAFE_DESTROY(FCopyParamScrollOnDragOver);
  SAFE_DESTROY(FCustomCommandsScrollOnDragOver);
  delete FEditorFont;
  delete FCustomCommandList;
  delete FCopyParamList;
  delete FEditorList;
}
//---------------------------------------------------------------------
bool __fastcall TPreferencesDialog::Execute(TPreferencesDialogData * DialogData)
{
  FDialogData = DialogData;
  LoadConfiguration();
  bool Result = (ShowModal() == mrOk);
  if (Result)
  {
    SaveConfiguration();
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::PrepareNavigationTree(TTreeView * Tree)
{
  Tree->FullExpand();
  int i = 0;
  while (i < Tree->Items->Count)
  {
    if ((!WinConfiguration->ExpertMode &&
         Tree->Items->Item[i]->SelectedIndex & 128))
    {
      Tree->Items->Delete(Tree->Items->Item[i]);
    }
    else
    {
      for (int pi = 0; pi < PageControl->PageCount; pi++)
      {
        if (PageControl->Pages[pi]->Tag == (Tree->Items->Item[i]->SelectedIndex & 127))
        {
          if (PageControl->Pages[pi]->Enabled)
          {
            // gets called multiple times occasionally
            // (e.g. when called from upload dialog invoked by /upload)
            if (!PageControl->Pages[pi]->Caption.IsEmpty())
            {
              Tree->Items->Item[i]->Text = PageControl->Pages[pi]->Caption;
              PageControl->Pages[pi]->Caption = L"";
            }
          }
          else
          {
            Tree->Items->Delete(Tree->Items->Item[i]);
            i--;
          }
          break;
        }
      }
      i++;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::LoadConfiguration()
{
  FNoUpdate++;
  try
  {
    if (FPreferencesMode != pmLogin)
    {
      LoggingFrame->LoadConfiguration();
      GeneralSettingsFrame->LoadConfiguration();
    }
    #define BOOLPROP(PROP) PROP ## Check->Checked = WinConfiguration->PROP;
    BOOLPROP(DefaultDirIsHome);
    BOOLPROP(PreservePanelState);
    BOOLPROP(DeleteToRecycleBin);
    BOOLPROP(DDTransferConfirmation);
    BOOLPROP(DDWarnLackOfTempSpace);
    BOOLPROP(ShowHiddenFiles);
    BOOLPROP(FormatSizeBytes);
    BOOLPROP(RenameWholeName);
    BOOLPROP(ShowInaccesibleDirectories);
    BOOLPROP(CopyOnDoubleClickConfirmation);
    BOOLPROP(ConfirmTransferring);
    BOOLPROP(ConfirmOverwriting);
    BOOLPROP(ConfirmResume);
    BOOLPROP(ConfirmDeleting);
    BOOLPROP(ConfirmRecycling);
    BOOLPROP(ConfirmClosingSession);
    BOOLPROP(ConfirmExitOnCompletion);
    BOOLPROP(ConfirmCommandSession);
    BOOLPROP(ContinueOnError);
    BOOLPROP(DDAllowMoveInit);
    BOOLPROP(BeepOnFinish);
    BOOLPROP(TemporaryDirectoryAppendSession);
    BOOLPROP(TemporaryDirectoryAppendPath);
    BOOLPROP(TemporaryDirectoryCleanup);
    BOOLPROP(ConfirmTemporaryDirectoryCleanup);
    BOOLPROP(FullRowSelect);

    if (WinConfiguration->DDTransferConfirmation == asAuto)
    {
      // allow greayed state only initially,
      // once the off state is confirmed, never allow returning
      // to the undefined state
      DDTransferConfirmationCheck->AllowGrayed = true;
    }
    CheckBoxAutoSwitchLoad(DDTransferConfirmationCheck, WinConfiguration->DDTransferConfirmation);

    BeepOnFinishAfterEdit->AsInteger =
      int(static_cast<double>(GUIConfiguration->BeepOnFinishAfter) * SecsPerDay);
    BOOLPROP(BalloonNotifications);

    DDExtEnabledButton->Checked = WinConfiguration->DDExtEnabled;
    DDExtDisabledButton->Checked = !DDExtEnabledButton->Checked;
    DDWarnOnMoveCheck->Checked = !WinConfiguration->DDAllowMove;

    if (WinConfiguration->DDTemporaryDirectory.IsEmpty())
    {
      DDSystemTemporaryDirectoryButton->Checked = true;
      DDTemporaryDirectoryEdit->Text = SystemTemporaryDirectory();
    }
    else
    {
      DDCustomTemporaryDirectoryButton->Checked = true;
      DDTemporaryDirectoryEdit->Text = WinConfiguration->DDTemporaryDirectory;
    }

    // Commander
    if (WinConfiguration->ScpCommander.NortonLikeMode == nlOff)
    {
      NortonLikeModeCombo->ItemIndex = 2;
    }
    else if (WinConfiguration->ScpCommander.NortonLikeMode == nlKeyboard)
    {
      NortonLikeModeCombo->ItemIndex = 1;
    }
    else
    {
      NortonLikeModeCombo->ItemIndex = 0;
    }

    SwappedPanelsCheck->Checked =
      WinConfiguration->ScpCommander.SwappedPanels;
    TreeOnLeftCheck->Checked = WinConfiguration->ScpCommander.TreeOnLeft;

    ExplorerKeyboardShortcutsCombo->ItemIndex =
      WinConfiguration->ScpCommander.ExplorerKeyboardShortcuts ? 1 : 0;
    BOOLPROP(UseLocationProfiles);

    CompareByTimeCheck->Checked = WinConfiguration->ScpCommander.CompareByTime;
    CompareBySizeCheck->Checked = WinConfiguration->ScpCommander.CompareBySize;

    // Local panel
    PreserveLocalDirectoryCheck->Checked =
      WinConfiguration->ScpCommander.PreserveLocalDirectory;
    SystemContextMenuCheck->Checked =
      WinConfiguration->ScpCommander.SystemContextMenu;

    // Explorer
    ShowFullAddressCheck->Checked =
      WinConfiguration->ScpExplorer.ShowFullAddress;

    RegistryStorageButton->Checked = (Configuration->Storage == stRegistry);
    IniFileStorageButton2->Checked = (Configuration->Storage != stRegistry);

    RandomSeedFileEdit->Text = Configuration->RandomSeedFile;

    // editor
    EditorWordWrapCheck->Checked = WinConfiguration->Editor.WordWrap;
    EditorTabSizeEdit->AsInteger = WinConfiguration->Editor.TabSize;
    if (WinConfiguration->Editor.Encoding == CP_UTF8)
    {
      EditorEncodingCombo->ItemIndex = 1;
    }
    else
    {
      EditorEncodingCombo->ItemIndex = 0;
    }
    FEditorFont->Name = WinConfiguration->Editor.FontName;
    FEditorFont->Height = WinConfiguration->Editor.FontHeight;
    FEditorFont->Charset = (TFontCharset)WinConfiguration->Editor.FontCharset;
    FEditorFont->Style = IntToFontStyles(WinConfiguration->Editor.FontStyle);
    (*FEditorList) = *WinConfiguration->EditorList;
    UpdateEditorListView();

    FCopyParams = GUIConfiguration->DefaultCopyParam;
    ResumeOnButton->Checked = GUIConfiguration->DefaultCopyParam.ResumeSupport == rsOn;
    ResumeSmartButton->Checked = GUIConfiguration->DefaultCopyParam.ResumeSupport == rsSmart;
    ResumeOffButton->Checked = GUIConfiguration->DefaultCopyParam.ResumeSupport == rsOff;
    ResumeThresholdEdit->Value = GUIConfiguration->DefaultCopyParam.ResumeThreshold / 1024;
    SessionReopenAutoCheck->Checked = (Configuration->SessionReopenAuto > 0);
    SessionReopenAutoEdit->Value = (Configuration->SessionReopenAuto > 0 ?
      (Configuration->SessionReopenAuto / MSecsPerSec) : 5);
    SessionReopenAutoIdleCheck->Checked = (GUIConfiguration->SessionReopenAutoIdle > 0);
    SessionReopenAutoIdleEdit->Value = (GUIConfiguration->SessionReopenAutoIdle > 0 ?
      (GUIConfiguration->SessionReopenAutoIdle / MSecsPerSec) : 5);
    SessionReopenAutoStallCheck->Checked = (Configuration->SessionReopenAutoStall > 0);
    SessionReopenAutoStallEdit->Value = (Configuration->SessionReopenAutoStall > 0 ?
      (Configuration->SessionReopenAutoStall / MSecsPerSec) : SecsPerMin);
    SessionReopenTimeoutEdit->Value = (Configuration->SessionReopenTimeout / MSecsPerSec);

    GeneralSheet->Enabled = (PreferencesMode != pmLogin) && WinConfiguration->ExpertMode;
    ExplorerSheet->Enabled = WinConfiguration->ExpertMode;
    CommanderSheet->Enabled = WinConfiguration->ExpertMode;
    GeneralSheet->Enabled = (PreferencesMode != pmLogin);
    EditorSheet->Enabled = WinConfiguration->ExpertMode && !WinConfiguration->DisableOpenEdit;

    StorageGroup->Visible = WinConfiguration->ExpertMode;
    RandomSeedFileLabel->Visible = WinConfiguration->ExpertMode;
    RandomSeedFileEdit->Visible = WinConfiguration->ExpertMode;

    FCustomCommandList->Assign(WinConfiguration->CustomCommandList);
    UpdateCustomCommandsView();

    PuttyPathEdit->Text = GUIConfiguration->PuttyPath;
    PuttyPasswordCheck2->Checked = GUIConfiguration->PuttyPassword;
    AutoOpenInPuttyCheck->Checked = WinConfiguration->AutoOpenInPutty;
    TelnetForFtpInPuttyCheck->Checked = WinConfiguration->TelnetForFtpInPutty;

    // Queue
    QueueTransferLimitEdit->AsInteger = GUIConfiguration->QueueTransfersLimit;
    EnableQueueByDefaultCheck->Checked = WinConfiguration->EnableQueueByDefault;
    QueueAutoPopupCheck->Checked = GUIConfiguration->QueueAutoPopup;
    QueueCheck->Checked = GUIConfiguration->DefaultCopyParam.Queue;
    QueueIndividuallyCheck->Checked = GUIConfiguration->DefaultCopyParam.QueueIndividually;
    QueueNoConfirmationCheck->Checked = GUIConfiguration->DefaultCopyParam.QueueNoConfirmation;
    RememberPasswordCheck->Checked = GUIConfiguration->QueueRememberPassword;
    if (!GUIConfiguration->QueueKeepDoneItems)
    {
      QueueKeepDoneItemsForCombo->ItemIndex = 0;
    }
    else if (GUIConfiguration->QueueKeepDoneItemsFor < 0)
    {
      QueueKeepDoneItemsForCombo->ItemIndex = 5;
    }
    else if (GUIConfiguration->QueueKeepDoneItemsFor <= 15)
    {
      QueueKeepDoneItemsForCombo->ItemIndex = 1;
    }
    else if (GUIConfiguration->QueueKeepDoneItemsFor <= 60)
    {
      QueueKeepDoneItemsForCombo->ItemIndex = 2;
    }
    else if (GUIConfiguration->QueueKeepDoneItemsFor <= 15 * 60)
    {
      QueueKeepDoneItemsForCombo->ItemIndex = 3;
    }
    else if (GUIConfiguration->QueueKeepDoneItemsFor <= 60 * 60)
    {
      QueueKeepDoneItemsForCombo->ItemIndex = 4;
    }
    if (WinConfiguration->QueueView.Show == qvShow)
    {
      QueueViewShowButton->Checked = true;
    }
    else if (WinConfiguration->QueueView.Show == qvHideWhenEmpty)
    {
      QueueViewHideWhenEmptyButton->Checked = true;
    }
    else
    {
      QueueViewHideButton->Checked = true;
    }

    // window
    AutoSaveWorkspaceCheck->Checked = WinConfiguration->AutoSaveWorkspace;
    AutoWorkspaceCombo->Text =
      (WinConfiguration->AutoWorkspace.IsEmpty() ?
        WinConfiguration->LastWorkspace : WinConfiguration->AutoWorkspace);
    AutoSaveWorkspacePasswordsCheck->Checked = WinConfiguration->AutoSaveWorkspacePasswords;
    if (WinConfiguration->PathInCaption == picFull)
    {
      PathInCaptionFullButton->Checked = true;
    }
    else if (WinConfiguration->PathInCaption == picShort)
    {
      PathInCaptionShortButton->Checked = true;
    }
    else
    {
      PathInCaptionNoneButton->Checked = true;
    }
    BOOLPROP(MinimizeToTray);

    // panels
    DoubleClickActionCombo->ItemIndex = WinConfiguration->DoubleClickAction;
    BOOLPROP(AutoReadDirectoryAfterOp);
    BOOLPROP(RefreshRemotePanel);
    RefreshRemotePanelIntervalEdit->Value =
      int(static_cast<double>(WinConfiguration->RefreshRemotePanelInterval) * SecsPerDay);

    // updates
    TUpdatesConfiguration Updates = WinConfiguration->Updates;
    if (int(Updates.Period) <= 0)
    {
      UpdatesPeriodCombo->ItemIndex = 0;
    }
    else if (int(Updates.Period) <= 1)
    {
      UpdatesPeriodCombo->ItemIndex = 1;
    }
    else if (int(Updates.Period) <= 7)
    {
      UpdatesPeriodCombo->ItemIndex = 2;
    }
    else
    {
      UpdatesPeriodCombo->ItemIndex = 3;
    }

    CollectUsageCheck->Checked = Configuration->CollectUsage;

    ComboAutoSwitchLoad(UpdatesBetaVersionsCombo, Updates.BetaVersions);

    switch (Updates.ConnectionType)
    {
      case ctDirect:
      default:
        UpdatesDirectCheck->Checked = true;
        break;

      case ctAuto:
        UpdatesAutoCheck->Checked = true;
        break;

      case ctProxy:
        UpdatesProxyCheck->Checked = true;
        break;
    }

    UpdatesProxyHostEdit->Text = Updates.ProxyHost;
    UpdatesProxyPortEdit->AsInteger = Updates.ProxyPort;

    // presets
    (*FCopyParamList) = *WinConfiguration->CopyParamList;
    UpdateCopyParamListView();
    CopyParamListView->ItemIndex = 0;
    BOOLPROP(CopyParamAutoSelectNotice);

    // interface
    if (WinConfiguration->Theme == L"OfficeXP")
    {
      ThemeCombo->ItemIndex = 1;
    }
    else if (WinConfiguration->Theme == L"Office2003")
    {
      ThemeCombo->ItemIndex = 2;
    }
    else
    {
      ThemeCombo->ItemIndex = 0;
    }

    // security
    UseMasterPasswordCheck->Checked = WinConfiguration->UseMasterPassword;

    // network
    RetrieveExternalIpAddressButton->Checked = Configuration->ExternalIpAddress.IsEmpty();
    CustomExternalIpAddressButton->Checked = !RetrieveExternalIpAddressButton->Checked;
    CustomExternalIpAddressEdit->Text = Configuration->ExternalIpAddress;
    TryFtpWhenSshFailsCheck->Checked = Configuration->TryFtpWhenSshFails;

    #undef BOOLPROP
  }
  __finally
  {
    FNoUpdate--;
  }

  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::SaveConfiguration()
{
  Configuration->BeginUpdate();
  try
  {
    TGUICopyParamType CopyParam = GUIConfiguration->DefaultCopyParam;

    if (FPreferencesMode != pmLogin)
    {
      LoggingFrame->SaveConfiguration();
      GeneralSettingsFrame->SaveConfiguration();
    }
    #define BOOLPROP(PROP) WinConfiguration->PROP = PROP ## Check->Checked
    BOOLPROP(DefaultDirIsHome);
    BOOLPROP(PreservePanelState);
    BOOLPROP(DeleteToRecycleBin);
    BOOLPROP(DDWarnLackOfTempSpace);
    BOOLPROP(ShowHiddenFiles);
    BOOLPROP(FormatSizeBytes);
    BOOLPROP(RenameWholeName);
    BOOLPROP(ShowInaccesibleDirectories);
    BOOLPROP(CopyOnDoubleClickConfirmation);
    BOOLPROP(ConfirmTransferring);
    BOOLPROP(ConfirmOverwriting);
    BOOLPROP(ConfirmResume);
    BOOLPROP(ConfirmDeleting);
    BOOLPROP(ConfirmRecycling);
    BOOLPROP(ConfirmClosingSession);
    BOOLPROP(ConfirmExitOnCompletion);
    BOOLPROP(ConfirmCommandSession);
    BOOLPROP(ContinueOnError);
    BOOLPROP(DDAllowMoveInit);
    BOOLPROP(BeepOnFinish);
    BOOLPROP(TemporaryDirectoryAppendSession);
    BOOLPROP(TemporaryDirectoryAppendPath);
    BOOLPROP(TemporaryDirectoryCleanup);
    BOOLPROP(ConfirmTemporaryDirectoryCleanup);
    BOOLPROP(FullRowSelect);

    WinConfiguration->DDTransferConfirmation =
      CheckBoxAutoSwitchSave(DDTransferConfirmationCheck);

    GUIConfiguration->BeepOnFinishAfter =
      static_cast<double>(BeepOnFinishAfterEdit->Value / SecsPerDay);
    BOOLPROP(BalloonNotifications);

    WinConfiguration->DDAllowMove = !DDWarnOnMoveCheck->Checked;
    WinConfiguration->DDExtEnabled = DDExtEnabledButton->Checked;

    if (DDSystemTemporaryDirectoryButton->Checked)
    {
      WinConfiguration->DDTemporaryDirectory = L"";
    }
    else
    {
      WinConfiguration->DDTemporaryDirectory = DDTemporaryDirectoryEdit->Text;
    }

    Configuration->Storage = RegistryStorageButton->Checked ? stRegistry : stIniFile;

    // Commander
    TScpCommanderConfiguration ScpCommander = WinConfiguration->ScpCommander;
    if (NortonLikeModeCombo->ItemIndex == 2)
    {
      ScpCommander.NortonLikeMode = nlOff;
    }
    else if (NortonLikeModeCombo->ItemIndex == 1)
    {
      ScpCommander.NortonLikeMode = nlKeyboard;
    }
    else
    {
      ScpCommander.NortonLikeMode = nlOn;
    }
    ScpCommander.SwappedPanels = SwappedPanelsCheck->Checked;
    ScpCommander.TreeOnLeft = TreeOnLeftCheck->Checked;

    ScpCommander.ExplorerKeyboardShortcuts =
      (ExplorerKeyboardShortcutsCombo->ItemIndex != 0);
    BOOLPROP(UseLocationProfiles);

    WinConfiguration->ScpCommander.CompareByTime = CompareByTimeCheck->Checked;
    WinConfiguration->ScpCommander.CompareBySize = CompareBySizeCheck->Checked;

    // Local panel
    ScpCommander.PreserveLocalDirectory = PreserveLocalDirectoryCheck->Checked;
    ScpCommander.SystemContextMenu = SystemContextMenuCheck->Checked;

    WinConfiguration->ScpCommander = ScpCommander;

    // Explorer
    TScpExplorerConfiguration ScpExplorer = WinConfiguration->ScpExplorer;
    ScpExplorer.ShowFullAddress = ShowFullAddressCheck->Checked;
    WinConfiguration->ScpExplorer = ScpExplorer;

    Configuration->RandomSeedFile = RandomSeedFileEdit->Text;

    // editor
    WinConfiguration->Editor.WordWrap = EditorWordWrapCheck->Checked;
    WinConfiguration->Editor.TabSize = EditorTabSizeEdit->AsInteger;
    switch (EditorEncodingCombo->ItemIndex)
    {
      case 1:
        WinConfiguration->Editor.Encoding = CP_UTF8;
        break;

      default:
        WinConfiguration->Editor.Encoding = CP_ACP;
        break;
    }
    WinConfiguration->Editor.FontName = FEditorFont->Name;
    WinConfiguration->Editor.FontHeight = FEditorFont->Height;
    WinConfiguration->Editor.FontCharset = FEditorFont->Charset;
    WinConfiguration->Editor.FontStyle = FontStylesToInt(FEditorFont->Style);
    WinConfiguration->EditorList = FEditorList;

    // overwrites only TCopyParamType fields
    CopyParam = FCopyParams;
    if (ResumeOnButton->Checked) CopyParam.ResumeSupport = rsOn;
    if (ResumeSmartButton->Checked) CopyParam.ResumeSupport = rsSmart;
    if (ResumeOffButton->Checked) CopyParam.ResumeSupport = rsOff;
    CopyParam.ResumeThreshold = ResumeThresholdEdit->AsInteger * 1024;

    Configuration->SessionReopenAuto =
      (SessionReopenAutoCheck->Checked ? (SessionReopenAutoEdit->AsInteger * MSecsPerSec) : 0);
    GUIConfiguration->SessionReopenAutoIdle =
      (SessionReopenAutoIdleCheck->Checked ? (SessionReopenAutoIdleEdit->AsInteger * MSecsPerSec) : 0);
    Configuration->SessionReopenAutoStall =
      (SessionReopenAutoStallCheck->Checked ? (SessionReopenAutoStallEdit->AsInteger * MSecsPerSec) : 0);
    Configuration->SessionReopenTimeout = (SessionReopenTimeoutEdit->AsInteger * MSecsPerSec);

    WinConfiguration->CustomCommandList = FCustomCommandList;

    GUIConfiguration->PuttyPath = PuttyPathEdit->Text;
    GUIConfiguration->PuttyPassword = PuttyPasswordCheck2->Checked;
    WinConfiguration->AutoOpenInPutty = AutoOpenInPuttyCheck->Checked;
    WinConfiguration->TelnetForFtpInPutty = TelnetForFtpInPuttyCheck->Checked;

    // Queue
    GUIConfiguration->QueueTransfersLimit = QueueTransferLimitEdit->AsInteger;
    WinConfiguration->EnableQueueByDefault = EnableQueueByDefaultCheck->Checked;
    GUIConfiguration->QueueAutoPopup = QueueAutoPopupCheck->Checked;
    CopyParam.Queue = QueueCheck->Checked;
    CopyParam.QueueIndividually = QueueIndividuallyCheck->Checked;
    CopyParam.QueueNoConfirmation = QueueNoConfirmationCheck->Checked;
    GUIConfiguration->QueueRememberPassword = RememberPasswordCheck->Checked;
    GUIConfiguration->QueueKeepDoneItems = (QueueKeepDoneItemsForCombo->ItemIndex != 0);
    switch (QueueKeepDoneItemsForCombo->ItemIndex)
    {
      case 0:
        GUIConfiguration->QueueKeepDoneItemsFor = 0;
        break;
      case 1:
        GUIConfiguration->QueueKeepDoneItemsFor = 15;
        break;
      case 2:
        GUIConfiguration->QueueKeepDoneItemsFor = 60;
        break;
      case 3:
        GUIConfiguration->QueueKeepDoneItemsFor = 15 * 60;
        break;
      case 4:
        GUIConfiguration->QueueKeepDoneItemsFor = 60 * 60;
        break;
      default:
        GUIConfiguration->QueueKeepDoneItemsFor = -1;
        break;
    }
    if (QueueViewShowButton->Checked)
    {
      WinConfiguration->QueueView.Show = qvShow;
    }
    else if (QueueViewHideWhenEmptyButton->Checked)
    {
      WinConfiguration->QueueView.Show = qvHideWhenEmpty;
    }
    else
    {
      WinConfiguration->QueueView.Show = qvHide;
    }

    GUIConfiguration->DefaultCopyParam = CopyParam;

    // window
    WinConfiguration->AutoSaveWorkspace =
      !AutoWorkspaceCombo->Text.IsEmpty() &&
      AutoSaveWorkspaceCheck->Checked;
    if (!AutoWorkspaceCombo->Text.IsEmpty())
    {
      WinConfiguration->AutoWorkspace = AutoWorkspaceCombo->Text;
    }
    WinConfiguration->AutoSaveWorkspacePasswords = AutoSaveWorkspacePasswordsCheck->Checked;
    if (PathInCaptionFullButton->Checked)
    {
       WinConfiguration->PathInCaption = picFull;
    }
    else if (PathInCaptionShortButton->Checked)
    {
      WinConfiguration->PathInCaption = picShort;
    }
    else
    {
      WinConfiguration->PathInCaption = picNone;
    }
    BOOLPROP(MinimizeToTray);

    // panels
    WinConfiguration->DoubleClickAction = (TDoubleClickAction)DoubleClickActionCombo->ItemIndex;
    BOOLPROP(AutoReadDirectoryAfterOp);
    BOOLPROP(RefreshRemotePanel);
    WinConfiguration->RefreshRemotePanelInterval =
      static_cast<double>(RefreshRemotePanelIntervalEdit->Value / SecsPerDay);

    // updates
    TUpdatesConfiguration Updates = WinConfiguration->Updates;
    if (UpdatesPeriodCombo->ItemIndex == 0)
    {
      Updates.Period = 0;
    }
    else if (UpdatesPeriodCombo->ItemIndex == 1)
    {
      Updates.Period = 1;
    }
    else if (UpdatesPeriodCombo->ItemIndex == 2)
    {
      Updates.Period = 7;
    }
    else
    {
      Updates.Period = 30;
    }

    Configuration->CollectUsage = CollectUsageCheck->Checked;

    Updates.BetaVersions = ComboAutoSwitchSave(UpdatesBetaVersionsCombo);

    if (UpdatesDirectCheck->Checked)
    {
      Updates.ConnectionType = ctDirect;
    }
    else if (UpdatesAutoCheck->Checked)
    {
      Updates.ConnectionType = ctAuto;
    }
    else if (UpdatesProxyCheck->Checked)
    {
      if (!UpdatesProxyHostEdit->Text.IsEmpty())
      {
        Updates.ConnectionType = ctProxy;
      }
      else
      {
        Updates.ConnectionType = ctDirect;
      }
    }
    Updates.ProxyHost = UpdatesProxyHostEdit->Text;
    Updates.ProxyPort = UpdatesProxyPortEdit->AsInteger;

    WinConfiguration->Updates = Updates;

    // presets
    WinConfiguration->CopyParamList = FCopyParamList;
    BOOLPROP(CopyParamAutoSelectNotice);

    // interface
    if (ThemeCombo->ItemIndex == 1)
    {
      WinConfiguration->Theme = L"OfficeXP";
    }
    else if (ThemeCombo->ItemIndex == 2)
    {
      WinConfiguration->Theme = L"Office2003";
    }
    else
    {
      WinConfiguration->Theme = L"Default";
    }

    // network
    Configuration->ExternalIpAddress =
      (CustomExternalIpAddressButton->Checked ? CustomExternalIpAddressEdit->Text : UnicodeString());
    Configuration->TryFtpWhenSshFails = TryFtpWhenSshFailsCheck->Checked;

    #undef BOOLPROP
  }
  __finally
  {
    Configuration->EndUpdate();
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::SetPreferencesMode(TPreferencesMode value)
{
  if (PreferencesMode != value)
  {
    FPreferencesMode = value;

    GeneralSheet->Enabled = (value != pmLogin);
    LogSheet->Enabled = (value != pmLogin);
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::FormShow(TObject * /*Sender*/)
{
  InstallPathWordBreakProc(RandomSeedFileEdit);
  InstallPathWordBreakProc(DDTemporaryDirectoryEdit);
  InstallPathWordBreakProc(PuttyPathEdit);

  PrepareNavigationTree(NavigationTree);

  switch (PreferencesMode) {
    case pmEditor: PageControl->ActivePage = EditorSheet; break;
    case pmCustomCommands: PageControl->ActivePage = CustomCommandsSheet; break;
    case pmQueue: PageControl->ActivePage = QueueSheet; break;
    case pmLogging: PageControl->ActivePage = LogSheet; break;
    case pmUpdates: PageControl->ActivePage = UpdatesSheet; break;
    case pmPresets: PageControl->ActivePage = CopyParamListSheet; break;
    case pmEditors: PageControl->ActivePage = EditorSheet; break;
    default: PageControl->ActivePage = PreferencesSheet; break;
  }
  PageControlChange(NULL);
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::ControlChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TPreferencesDialog::TabSample(UnicodeString Values)
{
  UnicodeString Result;
  for (int Index = 1; Index <= Values.Length(); Index++)
  {
    if (Index > 1)
    {
      Result += L' ';
      if (EditorTabSizeEdit->AsInteger > 2)
      {
        Result += UnicodeString::StringOfChar(L' ', EditorTabSizeEdit->AsInteger - 2);
      }
    }

    Result += Values[Index];
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::UpdateControls()
{
  if (FNoUpdate == 0)
  {
    EnableControl(BeepOnFinishAfterEdit, BeepOnFinishCheck->Checked);
    EnableControl(BeepOnFinishAfterText, BeepOnFinishCheck->Checked);
    EnableControl(BalloonNotificationsCheck, ::TTrayIcon::SupportsBalloons());

    EnableControl(ResumeThresholdEdit, ResumeSmartButton->Checked);
    EnableControl(ResumeThresholdUnitLabel, ResumeThresholdEdit->Enabled);
    EnableControl(SessionReopenAutoEdit, SessionReopenAutoCheck->Checked);
    EnableControl(SessionReopenAutoLabel, SessionReopenAutoEdit->Enabled);
    EnableControl(SessionReopenAutoSecLabel, SessionReopenAutoEdit->Enabled);
    EnableControl(SessionReopenAutoIdleEdit, SessionReopenAutoIdleCheck->Checked);
    EnableControl(SessionReopenAutoIdleLabel, SessionReopenAutoIdleEdit->Enabled);
    EnableControl(SessionReopenAutoIdleSecLabel, SessionReopenAutoIdleEdit->Enabled);
    EnableControl(SessionReopenAutoStallEdit, SessionReopenAutoStallCheck->Checked);
    EnableControl(SessionReopenAutoStallLabel, SessionReopenAutoStallEdit->Enabled);
    EnableControl(SessionReopenAutoStallSecLabel, SessionReopenAutoStallEdit->Enabled);
    EnableControl(SessionReopenTimeoutEdit,
      SessionReopenAutoEdit->Enabled || SessionReopenAutoStallCheck->Checked);
    EnableControl(SessionReopenTimeoutLabel, SessionReopenTimeoutEdit->Enabled);
    EnableControl(SessionReopenTimeoutSecLabel,SessionReopenTimeoutEdit->Enabled);

    EnableControl(CopyOnDoubleClickConfirmationCheck,
      (DoubleClickActionCombo->ItemIndex == 1) && ConfirmTransferringCheck->Checked);
    EnableControl(RefreshRemotePanelIntervalEdit, RefreshRemotePanelCheck->Checked);
    EnableControl(RefreshRemoteDirectoryUnitLabel, RefreshRemotePanelCheck->Checked);

    UnicodeString EditorFontLabelText;
    EditorFontLabelText = FMTLOAD(EDITOR_FONT_FMT,
      (FEditorFont->Name, FEditorFont->Size)) + L"\n\n";
    EditorFontLabelText += TabSample(L"ABCD") + L"\n";
    EditorFontLabelText += TabSample(L"1234");
    EditorFontLabel->Caption = EditorFontLabelText;
    EditorFontLabel->Font = FEditorFont;

    bool CommandSelected = (CustomCommandsView->Selected != NULL);
    EnableControl(EditCommandButton, CommandSelected);
    EnableControl(RemoveCommandButton, CommandSelected);
    EnableControl(UpCommandButton, CommandSelected &&
      CustomCommandsView->ItemIndex > 0);
    EnableControl(DownCommandButton, CommandSelected &&
      (CustomCommandsView->ItemIndex < CustomCommandsView->Items->Count - 1));

    bool CopyParamSelected = (CopyParamListView->Selected != NULL);
    EnableControl(EditCopyParamButton, CopyParamSelected);
    EnableControl(DuplicateCopyParamButton,
      CopyParamSelected && (CopyParamListView->ItemIndex >= 1));
    EnableControl(RemoveCopyParamButton,
      CopyParamSelected && (CopyParamListView->ItemIndex >= 1));
    EnableControl(UpCopyParamButton,
      CopyParamSelected && (CopyParamListView->ItemIndex > 1));
    EnableControl(DownCopyParamButton,
      CopyParamSelected &&
      (CopyParamListView->ItemIndex >= 1) &&
      (CopyParamListView->ItemIndex < CopyParamListView->Items->Count - 1));
    EnableControl(CopyParamAutoSelectNoticeCheck, FCopyParamList->AnyRule);

    UnicodeString InfoStr;
    if (CopyParamSelected)
    {
      const TCopyParamType * SelectedCopyParam = GetCopyParam(CopyParamListView->ItemIndex);
      InfoStr = SelectedCopyParam->GetInfoStr(L"; ", 0);
      if (CopyParamListView->ItemIndex >= 1)
      {
        const TCopyParamRule * Rule = FCopyParamList->Rules[CopyParamListView->ItemIndex - 1];
        if (Rule != NULL)
        {
          InfoStr += L"\n" + FORMAT(ReplaceStr(LoadStr(COPY_PARAM_RULE), L"\n", L" "), (Rule->GetInfoStr(L"; ")));
        }
      }
    }
    CopyParamLabel->Caption = InfoStr;
    CopyParamLabel->Hint = InfoStr;
    CopyParamLabel->ShowHint =
      (CopyParamLabel->Canvas->TextWidth(InfoStr) > (CopyParamLabel->Width * 3 / 2));

    EnableControl(DDExtEnabledButton, WinConfiguration->DDExtInstalled);
    EnableControl(DDExtEnabledLabel, WinConfiguration->DDExtInstalled);
    EnableControl(DDExtDisabledPanel, DDExtDisabledButton->Checked);
    EnableControl(DDTemporaryDirectoryEdit, DDCustomTemporaryDirectoryButton->Enabled &&
      DDCustomTemporaryDirectoryButton->Checked);
    EnableControl(DDWarnOnMoveCheck, DDExtDisabledButton->Checked &&
      DDAllowMoveInitCheck->Checked);
    EnableControl(ConfirmTemporaryDirectoryCleanupCheck,
      TemporaryDirectoryCleanupCheck->Checked);
    IniFileStorageButton2->Caption =
      AnsiReplaceStr(IniFileStorageButton2->Caption, L"winscp.ini",
        ExtractFileName(ExpandEnvironmentVariables(Configuration->IniFileStorageNameForReading)));

    EditorFontLabel->WordWrap = EditorWordWrapCheck->Checked;
    bool EditorSelected = (EditorListView2->Selected != NULL);
    EnableControl(EditEditorButton, EditorSelected);
    EnableControl(RemoveEditorButton, EditorSelected);
    EnableControl(UpEditorButton, EditorSelected &&
      (EditorListView2->ItemIndex > 0));
    EnableControl(DownEditorButton, EditorSelected &&
      (EditorListView2->ItemIndex < EditorListView2->Items->Count - 1));

    EnableControl(UsageViewButton, CollectUsageCheck->Checked);
    EnableControl(UpdatesProxyHostEdit, UpdatesProxyCheck->Checked);
    EnableControl(UpdatesProxyHostLabel, UpdatesProxyHostEdit->Enabled);
    EnableControl(UpdatesProxyPortEdit, UpdatesProxyCheck->Checked);
    EnableControl(UpdatesProxyPortLabel, UpdatesProxyPortEdit->Enabled);

    EnableControl(PuttyPasswordCheck2, !PuttyPathEdit->Text.IsEmpty());
    EnableControl(AutoOpenInPuttyCheck, PuttyPasswordCheck2->Enabled);
    EnableControl(TelnetForFtpInPuttyCheck, PuttyPasswordCheck2->Enabled);

    EnableControl(SetMasterPasswordButton, WinConfiguration->UseMasterPassword);

    // network
    EnableControl(CustomExternalIpAddressEdit, CustomExternalIpAddressButton->Checked);

    // window
    EnableControl(AutoWorkspaceCombo, AutoSaveWorkspaceCheck->Checked);
    EnableControl(AutoSaveWorkspacePasswordsCheck,
      !Configuration->DisablePasswordStoring &&
      AutoWorkspaceCombo->Enabled);

    // integration
    // There's no quick launch in Windows 7
    EnableControl(QuickLaunchIconButton, !IsWin7());
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::EditorFontButtonClick(TObject * /*Sender*/)
{
  if (FontDialog(FEditorFont))
  {
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::FormCloseQuery(TObject * /*Sender*/,
  bool & /*CanClose*/)
{
  if (ModalResult != mrCancel)
  {
    ExitActiveControl(this);
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::IconButtonClick(TObject *Sender)
{
  UnicodeString IconName, Params;
  int SpecialFolder;

  if (Sender == DesktopIconButton)
  {
    IconName = AppName;
    int Result =
      MessageDialog(LoadStr(CREATE_DESKTOP_ICON), qtConfirmation,
        qaYes | qaNo | qaCancel, HELP_CREATE_ICON);
    switch (Result)
    {
      case qaYes:
        SpecialFolder = CSIDL_COMMON_DESKTOPDIRECTORY;
        break;

      case qaNo:
        SpecialFolder = CSIDL_DESKTOPDIRECTORY;
        break;

      default:
        Abort();
        break;
    }
  }
  else
  {
    if (MessageDialog(LoadStr(CONFIRM_CREATE_ICON),
          qtConfirmation, qaYes | qaNo, HELP_CREATE_ICON) == qaYes)
    {
      if (Sender == SendToHookButton)
      {
        IconName = FMTLOAD(SENDTO_HOOK_NAME, (AppName));
        SpecialFolder = CSIDL_SENDTO;
        Params = L"/upload";
      }
      else if (Sender == QuickLaunchIconButton)
      {
        IconName = L"Microsoft\\Internet Explorer\\Quick Launch\\" +
          AppName;
        SpecialFolder = CSIDL_APPDATA;
      }
    }
    else
    {
      Abort();
    }
  }

  CreateDesktopShortCut(IconName,
    Application->ExeName, Params, L"", SpecialFolder);
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::CustomCommandsViewData(TObject * /*Sender*/,
      TListItem * Item)
{
  assert(FCustomCommandList != NULL);
  int Index = Item->Index;
  assert(Index >= 0 && Index <= FCustomCommandList->Count);
  const TCustomCommandType * Command = FCustomCommandList->Commands[Index];
  UnicodeString Caption = StripHotkey(Command->Name);
  if (Command->ShortCut != 0)
  {
    Caption = FORMAT(L"%s (%s)", (Caption, ShortCutToText(Command->ShortCut)));
  }
  Item->Caption = Caption;
  assert(!Item->SubItems->Count);
  Item->SubItems->Add(Command->Command);
  int Params = Command->Params;
  Item->SubItems->Add(LoadStr(
    FLAGSET(Params, ccLocal) ? CUSTOM_COMMAND_LOCAL : CUSTOM_COMMAND_REMOTE));
  UnicodeString ParamsStr;
  #define ADDPARAM(PARAM, STR) \
    if (FLAGSET(Params, PARAM)) \
      ParamsStr += (ParamsStr.IsEmpty() ? L"" : L"/") + LoadStr(STR);
  ADDPARAM(ccApplyToDirectories, CUSTOM_COMMAND_DIRECTORIES);
  ADDPARAM(ccRecursive, CUSTOM_COMMAND_RECURSE);
  #undef ADDPARAM
  Item->SubItems->Add(ParamsStr);
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::ListViewSelectItem(
  TObject * /*Sender*/, TListItem * /*Item*/, bool /*Selected*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::UpdateCustomCommandsView()
{
  CustomCommandsView->Items->Count = FCustomCommandList->Count;
  AdjustListColumnsWidth(CustomCommandsView, FCustomCommandList->Count);
  CustomCommandsView->Invalidate();
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::CustomCommandsViewKeyDown(
      TObject * /*Sender*/, WORD & Key, TShiftState /*Shift*/)
{
  if (RemoveCommandButton->Enabled && (Key == VK_DELETE))
  {
    RemoveCommandButtonClick(NULL);
  }

  if (AddCommandButton->Enabled && (Key == VK_INSERT))
  {
    AddEditCommandButtonClick(AddCommandButton);
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::CustomCommandsViewDblClick(
  TObject * /*Sender*/)
{
  if (EditCommandButton->Enabled)
  {
    AddEditCommandButtonClick(EditCommandButton);
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::AddEditCommandButtonClick(TObject * Sender)
{
  bool Edit = (Sender == EditCommandButton);
  TCustomCommandType Command;

  if (Edit)
  {
    int Index = CustomCommandsView->ItemIndex;
    assert(Index >= 0 && Index <= FCustomCommandList->Count);

    Command = *FCustomCommandList->Commands[Index];
  }

  TShortCuts ShortCuts;
  if (WinConfiguration->SharedBookmarks != NULL)
  {
    WinConfiguration->SharedBookmarks->ShortCuts(ShortCuts);
  }
  FCustomCommandList->ShortCuts(ShortCuts);

  if (DoCustomCommandDialog(Command, FCustomCommandList,
        (Edit ? ccmEdit : ccmAdd), 0, NULL, &ShortCuts))
  {
    int Index = CustomCommandsView->ItemIndex;
    TCustomCommandType * ACommand = new TCustomCommandType(Command);
    if (Edit)
    {
      FCustomCommandList->Change(Index, ACommand);
    }
    else
    {
      if (Index >= 0)
      {
        FCustomCommandList->Insert(Index, ACommand);
      }
      else
      {
        FCustomCommandList->Add(ACommand);
        Index = FCustomCommandList->Count - 1;
      }
    }

    UpdateCustomCommandsView();
    CustomCommandsView->ItemIndex = Index;
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::RemoveCommandButtonClick(
      TObject * /*Sender*/)
{
  assert(CustomCommandsView->ItemIndex >= 0 &&
    CustomCommandsView->ItemIndex < FCustomCommandList->Count);
  FCustomCommandList->Delete(CustomCommandsView->ItemIndex);
  UpdateCustomCommandsView();
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::CustomCommandMove(int Source, int Dest)
{
  if (Source >= 0 && Source < FCustomCommandList->Count &&
      Dest >= 0 && Dest < FCustomCommandList->Count)
  {
    FCustomCommandList->Move(Source, Dest);
    // workaround for bug in VCL
    CustomCommandsView->ItemIndex = -1;
    CustomCommandsView->ItemFocused = CustomCommandsView->Selected;
    CustomCommandsView->ItemIndex = Dest;
    UpdateCustomCommandsView();
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::UpDownCommandButtonClick(TObject * Sender)
{
  CustomCommandMove(CustomCommandsView->ItemIndex,
    CustomCommandsView->ItemIndex + (Sender == UpCommandButton ? -1 : 1));
}
//---------------------------------------------------------------------------
TListViewScrollOnDragOver * __fastcall TPreferencesDialog::ScrollOnDragOver(TObject * ListView)
{
  if (ListView == CopyParamListView)
  {
    return FCopyParamScrollOnDragOver;
  }
  else if (ListView == CustomCommandsView)
  {
    return FCustomCommandsScrollOnDragOver;
  }
  else if (ListView == EditorListView2)
  {
    return FEditorScrollOnDragOver;
  }
  else
  {
    assert(false);
    return NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::ListViewStartDrag(
      TObject * Sender, TDragObject *& /*DragObject*/)
{
  FListViewDragSource = dynamic_cast<TListView*>(Sender)->ItemIndex;
  FListViewDragDest = -1;
  ScrollOnDragOver(Sender)->StartDrag();
}
//---------------------------------------------------------------------------
bool __fastcall TPreferencesDialog::AllowListViewDrag(TObject * Sender, int X, int Y)
{
  TListItem * Item = dynamic_cast<TListView*>(Sender)->GetItemAt(X, Y);
  FListViewDragDest = Item ? Item->Index : -1;
  return (FListViewDragDest >= 0) && (FListViewDragDest != FListViewDragSource);
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::CustomCommandsViewDragDrop(
      TObject * Sender, TObject * Source, int X, int Y)
{
  if (Source == CustomCommandsView)
  {
    if (AllowListViewDrag(Sender, X, Y))
    {
      CustomCommandMove(FListViewDragSource, FListViewDragDest);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::ListViewDragOver(
  TObject * Sender, TObject * Source, int X, int Y,
  TDragState /*State*/, bool & Accept)
{
  if (Source == Sender)
  {
    // cannot use AllowListViewDrag(X, Y) because of bug in VCL
    // (when dropped on item itself, when it was dragged over another item before,
    // that another item remains highlighted forever)
    Accept = true;

    ScrollOnDragOver(Source)->DragOver(TPoint(X, Y));
  }
}
//---------------------------------------------------------------------------
const TCopyParamType * TPreferencesDialog::GetCopyParam(int Index)
{
  if (Index == 0)
  {
    return &FCopyParams;
  }
  else
  {
    return FCopyParamList->CopyParams[Index - 1];
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::CopyParamMove(int Source, int Dest)
{
  if (Source >= 1 && Source < (1 + FCopyParamList->Count) &&
      Dest >= 0 && Dest < (1 + FCopyParamList->Count))
  {
    if (Dest == 0)
    {
      Dest = 1;
    }
    FCopyParamList->Move(Source - 1, Dest - 1);
    // workaround for bug in VCL
    CopyParamListView->ItemIndex = -1;
    CopyParamListView->ItemFocused = CopyParamListView->Selected;
    CopyParamListView->ItemIndex = Dest;
    UpdateCopyParamListView();
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::CopyParamListViewDragDrop(
  TObject * Sender, TObject * Source, int X, int Y)
{
  if (Source == CopyParamListView)
  {
    if (AllowListViewDrag(Sender, X, Y))
    {
      CopyParamMove(FListViewDragSource, FListViewDragDest);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::UpDownCopyParamButtonClick(TObject * Sender)
{
  CopyParamMove(CopyParamListView->ItemIndex,
    CopyParamListView->ItemIndex + (Sender == UpCopyParamButton ? -1 : 1));
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::RemoveCopyParamButtonClick(
  TObject * /*Sender*/)
{
  assert(CopyParamListView->ItemIndex >= 1 &&
    CopyParamListView->ItemIndex < (1 + FCopyParamList->Count));
  FCopyParamList->Delete(CopyParamListView->ItemIndex - 1);
  UpdateCopyParamListView();
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::AddEditCopyParam(TCopyParamPresetMode Mode)
{
  int Index = CopyParamListView->ItemIndex;
  bool Result;
  if ((Index == 0) && (Mode != cpmAdd))
  {
    Result = DoCopyParamCustomDialog(FCopyParams, 0);
  }
  else
  {
    if (Index == 0)
    {
      assert(Mode == cpmAdd);
      Index = 1;
    }

    TCopyParamRuleData * CopyParamRuleData =
      (FDialogData != NULL ? FDialogData->CopyParamRuleData : NULL);
    Index--;
    Result = DoCopyParamPresetDialog(FCopyParamList, Index, Mode, CopyParamRuleData);
    if (Result)
    {
      UpdateCopyParamListView();
      CopyParamListView->ItemIndex = Index + 1;
      // when using duplicate button, focus remains on original item
      CopyParamListView->ItemFocused = CopyParamListView->Selected;
    }
  }

  if (Result)
  {
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::AddCopyParamButtonClick(TObject * /*Sender*/)
{
  AddEditCopyParam(cpmAdd);
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::EditCopyParamButtonClick(TObject * /*Sender*/)
{
  AddEditCopyParam(cpmEdit);
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::DuplicateCopyParamButtonClick(TObject * /*Sender*/)
{
  if (ALWAYS_TRUE(CopyParamListView->ItemIndex >= 1))
  {
    AddEditCopyParam(cpmDuplicate);
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::CopyParamListViewDblClick(
  TObject * /*Sender*/)
{
  if (EditCopyParamButton->Enabled)
  {
    AddEditCopyParam(cpmEdit);
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::CopyParamListViewKeyDown(
  TObject * /*Sender*/, WORD & Key, TShiftState /*Shift*/)
{
  if (RemoveCopyParamButton->Enabled && (Key == VK_DELETE))
  {
    RemoveCopyParamButtonClick(NULL);
  }

  if (AddCopyParamButton->Enabled && (Key == VK_INSERT))
  {
    AddEditCopyParam(cpmAdd);
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::EditorMove(int Source, int Dest)
{
  if (Source >= 0 && Source < FEditorList->Count &&
      Dest >= 0 && Dest < FEditorList->Count)
  {
    FEditorList->Move(Source, Dest);
    // workaround for bug in VCL
    EditorListView2->ItemIndex = -1;
    EditorListView2->ItemFocused = EditorListView2->Selected;
    EditorListView2->ItemIndex = Dest;
    UpdateEditorListView();
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::EditorListView2DragDrop(TObject * Sender,
  TObject * Source, int X, int Y)
{
  if (Source == EditorListView2)
  {
    if (AllowListViewDrag(Sender, X, Y))
    {
      EditorMove(FListViewDragSource, FListViewDragDest);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::UpDownEditorButtonClick(TObject *Sender)
{
  EditorMove(EditorListView2->ItemIndex,
    EditorListView2->ItemIndex + (Sender == UpEditorButton ? -1 : 1));
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::RemoveEditorButtonClick(
  TObject * /*Sender*/)
{
  assert(EditorListView2->ItemIndex >= 0 &&
    EditorListView2->ItemIndex < FEditorList->Count);
  FEditorList->Delete(EditorListView2->ItemIndex);
  UpdateEditorListView();
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::AddEditEditorButtonClick(TObject * Sender)
{
  TEditorPreferencesMode Mode = (Sender == EditEditorButton ? epmEdit : epmAdd);
  int Index = EditorListView2->ItemIndex;
  TEditorPreferences * Editor;
  if (Mode == epmEdit)
  {
    Editor = new TEditorPreferences(*FEditorList->Editors[Index]);
  }
  else
  {
    Editor = new TEditorPreferences();
  }

  try
  {
    bool DummyRemember = false;
    if (DoEditorPreferencesDialog(Editor->GetData(), DummyRemember, Mode, true))
    {
      if (Mode == epmEdit)
      {
        FEditorList->Change(Index, Editor);
      }
      else
      {
        if (Index < 0)
        {
          Index = FEditorList->Count;
          FEditorList->Add(Editor);
        }
        else
        {
          FEditorList->Insert(Index, Editor);
        }
      }
      // ownership of the object lost
      Editor = NULL;

      UpdateEditorListView();
      EditorListView2->ItemIndex = Index;
      UpdateControls();
    }
  }
  __finally
  {
    delete Editor;
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::EditorListView2DblClick(TObject * /*Sender*/)
{
  if (EditEditorButton->Enabled)
  {
    AddEditEditorButtonClick(EditEditorButton);
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::EditorListView2KeyDown(TObject * /*Sender*/,
  WORD & Key, TShiftState /*Shift*/)
{
  if (RemoveEditorButton->Enabled && (Key == VK_DELETE))
  {
    RemoveEditorButtonClick(NULL);
  }

  if (AddEditorButton->Enabled && (Key == VK_INSERT))
  {
    AddEditEditorButtonClick(AddEditorButton);
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::UpdateEditorListView()
{
  EditorListView2->Items->Count = FEditorList->Count;
  AdjustListColumnsWidth(EditorListView2, FEditorList->Count);
  EditorListView2->Invalidate();
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::EditorListView2Data(TObject * /*Sender*/,
  TListItem * Item)
{
  int Index = Item->Index;
  assert(Index >= 0 && Index <= FEditorList->Count);
  const TEditorPreferences * Editor = FEditorList->Editors[Index];
  Item->Caption = Editor->Name;
  Item->SubItems->Add(Editor->Data->FileMask.Masks);
  if (Editor->Data->Editor == edExternal)
  {
    Item->SubItems->Add(BooleanToStr(Editor->Data->ExternalEditorText));
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::NavigationTreeChange(TObject * /*Sender*/,
      TTreeNode *Node)
{
  if (Node->SelectedIndex)
  {
    for (Integer Index = 0; Index < PageControl->PageCount; Index++)
    {
      if (PageControl->Pages[Index]->Tag == (Node->SelectedIndex & 127))
      {
        PageControl->ActivePage = PageControl->Pages[Index];
        // reshow the accelerators, etc
        ResetSystemSettings(this);
        // This is particularly here to enable EditCopyParamButton,
        // as to some reason CopyParamListView->Selected is NULL until
        // its page is shown for the first time
        UpdateControls();
        return;
      }
    }
  }
  assert(false);
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::PageControlChange(TObject * /*Sender*/)
{
  bool Found = false;
  if (PageControl->ActivePage->Tag)
  {
    for (int Index = 0; Index < NavigationTree->Items->Count; Index++)
    {
      if ((NavigationTree->Items->Item[Index]->SelectedIndex & 127) ==
            PageControl->ActivePage->Tag)
      {
        NavigationTree->Items->Item[Index]->Selected = true;
        Found = true;
      }
    }
  }

  assert(Found);
  if (Found)
  {
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::CMDialogKey(TWMKeyDown & Message)
{
  if (Message.CharCode == VK_TAB)
  {
    TShiftState Shift = KeyDataToShiftState(Message.KeyData);
    if (Shift.Contains(ssCtrl))
    {
      TTreeNode * Node = NavigationTree->Selected;
      if (!Shift.Contains(ssShift))
      {
        Node = Node->GetNext();
        if (!Node) Node = NavigationTree->Items->GetFirstNode();
      }
      else
      {
        if (Node->GetPrev()) Node = Node->GetPrev();
          else
        while (Node->GetNext()) Node = Node->GetNext();
      }
      Node->Selected = True;
      Message.Result = 1;
      return;
    }
  }
  TForm::Dispatch(&Message);
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::WMHelp(TWMHelp & Message)
{
  assert(Message.HelpInfo != NULL);

  if (Message.HelpInfo->iContextType == HELPINFO_WINDOW)
  {
    // invoke help for active page (not for whole form), regardless of focus
    // (e.g. even if focus is on control outside pagecontrol)
    Message.HelpInfo->hItemHandle = PageControl->ActivePage->Handle;
  }
  TForm::Dispatch(&Message);
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::Dispatch(void *Message)
{
  TMessage * M = reinterpret_cast<TMessage*>(Message);
  assert(M);
  if (M->Msg == CM_DIALOGKEY)
  {
    CMDialogKey(*((TWMKeyDown *)Message));
  }
  else if (M->Msg == WM_HELP)
  {
    WMHelp(*((TWMHelp *)Message));
  }
  else
  {
    TForm::Dispatch(Message);
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::RegisterAsUrlHandlerButtonClick(
  TObject * /*Sender*/)
{
  if (MessageDialog(LoadStr(CONFIRM_REGISTER_URL),
        qtConfirmation, qaYes | qaNo, HELP_REGISTER_URL) == qaYes)
  {
    RegisterAsUrlHandler();
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::DDExtLabelClick(TObject * Sender)
{
  ((Sender == DDExtEnabledLabel) ? DDExtEnabledButton : DDExtDisabledButton)->
    SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::AddSearchPathButtonClick(
  TObject * /*Sender*/)
{
  UnicodeString AppPath = ExtractFilePath(Application->ExeName);
  if (MessageDialog(FMTLOAD(CONFIRM_ADD_SEARCH_PATH, (AppPath)),
        qtConfirmation, qaYes | qaNo, HELP_ADD_SEARCH_PATH) == qaYes)
  {
    AddSearchPath(AppPath);
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::EditorFontLabelDblClick(
  TObject * Sender)
{
  EditorFontButtonClick(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::UpdateCopyParamListView()
{
  CopyParamListView->Items->Count = 1 + FCopyParamList->Count;
  AdjustListColumnsWidth(CopyParamListView, 1 + FCopyParamList->Count);
  CopyParamListView->Invalidate();
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::CopyParamListViewData(TObject * /*Sender*/,
  TListItem * Item)
{
  UnicodeString Name;
  UnicodeString Rule;

  int Index = Item->Index;
  if (Index == 0)
  {
    Name = StripHotkey(LoadStr(COPY_PARAM_DEFAULT));
  }
  else
  {
    assert(Index >= 1 && Index <= 1 + FCopyParamList->Count);
    Name = StripHotkey(FCopyParamList->Names[Index - 1]);
    Rule = BooleanToStr(FCopyParamList->Rules[Index - 1] != NULL);
  }

  Item->Caption = Name;
  Item->SubItems->Add(Rule);
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::PuttyPathBrowseButtonClick(
  TObject * /*Sender*/)
{
  BrowseForExecutable(PuttyPathEdit, LoadStr(PREFERENCES_SELECT_PUTTY),
    LoadStr(PREFERENCES_PUTTY_FILTER), false, false);
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::PuttyPathResetButtonClick(
  TObject * /*Sender*/)
{
  PuttyPathEdit->Text = WinConfiguration->DefaultPuttyPath;
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::PathEditBeforeDialog(
  TObject * /*Sender*/, UnicodeString & Name, bool & /*Action*/)
{
  FBeforeDialogPath = Name;
  Name = ExpandEnvironmentVariables(Name);
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::PathEditAfterDialog(
  TObject * /*Sender*/, UnicodeString & Name, bool & /*Action*/)
{
  if (CompareFileName(Name, ExpandEnvironmentVariables(FBeforeDialogPath)))
  {
    Name = FBeforeDialogPath;
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::NavigationTreeCollapsing(
  TObject * /*Sender*/, TTreeNode * /*Node*/, bool & AllowCollapse)
{
  AllowCollapse = false;
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::ListViewEndDrag(
  TObject * Sender, TObject * /*Target*/, int /*X*/, int /*Y*/)
{
  ScrollOnDragOver(Sender)->EndDrag();
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::RandomSeedFileEditCreateEditDialog(
  TObject * Sender, TFileDialogKind DialogKind, TOpenDialog *& Dialog)
{
  USEDPARAM(DialogKind);
  assert(DialogKind == dkOpen);
  Dialog = new TOpenDialog(dynamic_cast<TComponent *>(Sender));
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::SessionReopenTimeoutEditSetValue(
  TObject * /*Sender*/, Extended Value, UnicodeString & Text, bool & Handled)
{
  if (Value == 0)
  {
    Text = LoadStr(PREFERENCES_RECONNECT_TIMEOUT_UNLIMITED);
    Handled = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::SessionReopenTimeoutEditGetValue(
  TObject * /*Sender*/, UnicodeString Text, Extended & Value, bool & Handled)
{
  if (AnsiSameText(Text, LoadStr(PREFERENCES_RECONNECT_TIMEOUT_UNLIMITED)))
  {
    Value = 0;
    Handled = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::UseMasterPasswordCheckClick(
  TObject * /*Sender*/)
{
  if (UseMasterPasswordCheck->Checked != WinConfiguration->UseMasterPassword)
  {
    try
    {
      if (UseMasterPasswordCheck->Checked)
      {
        if (DoChangeMasterPasswordDialog())
        {
          MessageDialog(LoadStr(MASTER_PASSWORD_SET), qtInformation, qaOK, HELP_MASTER_PASSWORD);
        }
      }
      else
      {
        if (DoMasterPasswordDialog())
        {
          WinConfiguration->ClearMasterPassword();
          MessageDialog(LoadStr(MASTER_PASSWORD_CLEARED), qtInformation, qaOK, HELP_MASTER_PASSWORD);
        }
      }
    }
    __finally
    {
      UseMasterPasswordCheck->Checked = WinConfiguration->UseMasterPassword;
      UpdateControls();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::SetMasterPasswordButtonClick(
  TObject * /*Sender*/)
{
  if (DoChangeMasterPasswordDialog())
  {
    MessageDialog(LoadStr(MASTER_PASSWORD_CHANGED), qtInformation, qaOK, HELP_MASTER_PASSWORD);
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::UsageViewButtonClick(TObject * /*Sender*/)
{
  TStrings * Data = new TStringList();
  try
  {
    Data->Text = GetUsageData();
    UnicodeString Message =
      LoadStr(Data->Text.IsEmpty() ? USAGE_DATA_NONE : USAGE_DATA);
    MoreMessageDialog(Message, Data, qtInformation, qaOK, HELP_USAGE);
  }
  __finally
  {
    delete Data;
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::CopyParamLabelClick(TObject * /*Sender*/)
{
  if (EditCopyParamButton->Enabled)
  {
    AddEditCopyParam(cpmEdit);
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::CopyParamListViewCustomDrawItem(
  TCustomListView * Sender, TListItem * Item,
  TCustomDrawState /*State*/, bool & /*DefaultDraw*/)
{
  if (Item->Index == 0)
  {
    Sender->Canvas->Font->Style = Sender->Canvas->Font->Style << fsBold;
  }
}
//---------------------------------------------------------------------------
