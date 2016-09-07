//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <StrUtils.hpp>
#include <Common.h>
#include <math.h>

#include "Preferences.h"
#include "Custom.h"

#include <CoreMain.h>
#include <Terminal.h>
#include <Bookmarks.h>

#include "VCLCommon.h"
#include "GUITools.h"
#include "Tools.h"
#include "TextsCore.h"
#include "TextsWin.h"
#include "HelpWin.h"
#include "WinInterface.h"
#include "WinConfiguration.h"
#include "Setup.h"
#include "ProgParams.h"
#include "Http.h"
//---------------------------------------------------------------------
#pragma link "CopyParams"
#pragma link "UpDownEdit"
#pragma link "ComboEdit"
#ifndef NO_RESOURCES
#pragma link "HistoryComboBox"
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------
bool __fastcall DoPreferencesDialog(TPreferencesMode APreferencesMode,
  TPreferencesDialogData * DialogData)
{
  bool Result;
  TPreferencesDialog * PreferencesDialog =
    new TPreferencesDialog(GetFormOwner(), APreferencesMode);
  try
  {
    Result = PreferencesDialog->Execute(DialogData);
    if (Result)
    {
      Configuration->SaveExplicit();
    }
  }
  __finally
  {
    delete PreferencesDialog;
  }
  return Result;
}
//---------------------------------------------------------------------
__fastcall TPreferencesDialog::TPreferencesDialog(
  TComponent* AOwner, TPreferencesMode PreferencesMode)
  : TForm(AOwner)
{
  SetCorrectFormParent(this);

  FNoUpdate = 0;
  FPreferencesMode = PreferencesMode;
  FEditorFont.reset(new TFont());
  FEditorFont->Color = clWindowText;
  FPanelFont.reset(new TFont());
  FPanelFont->Color = clWindowText;
  // currently useless
  FAfterFilenameEditDialog = false;
  FCustomCommandList = new TCustomCommandList();
  FCustomCommandChanging = false;
  FExtensionList = new TCustomCommandList();
  FListViewDragDest = -1;
  FCopyParamList = new TCopyParamList();
  FEditorList = new TEditorList();
  FAutomaticUpdatesPossible = IsInstalled();
  FCustomCommandsHintItem = NULL;
  FAddedExtensions.reset(CreateSortedStringList());
  FCustomCommandOptions.reset(new TStringList());
  UseSystemSettings(this);

  FCustomCommandsScrollOnDragOver = new TListViewScrollOnDragOver(CustomCommandsView, true);
  FixListColumnWidth(CustomCommandsView, -1);
  FCopyParamScrollOnDragOver = new TListViewScrollOnDragOver(CopyParamListView, true);
  FixListColumnWidth(CopyParamListView, -1);
  FEditorScrollOnDragOver = new TListViewScrollOnDragOver(EditorListView3, true);
  FixListColumnWidth(EditorListView3, -1);

  FOrigCustomCommandsViewWindowProc = CustomCommandsView->WindowProc;
  CustomCommandsView->WindowProc = CustomCommandsViewWindowProc;

  ComboAutoSwitchInitialize(UpdatesBetaVersionsCombo);
  EnableControl(UpdatesBetaVersionsCombo, !WinConfiguration->IsBeta);
  EnableControl(UpdatesBetaVersionsLabel, UpdatesBetaVersionsCombo->Enabled);

  HintLabel(LogFileNameHintText, LoadStr(LOG_FILE_HINT3));
  HintLabel(ActionsLogFileNameHintText, LoadStr(LOG_FILE_HINT3));

  HintLabel(ShellIconsText2);
  HotTrackLabel(CopyParamLabel);
  HintLabel(PuttyPathHintText, LoadStr(PUTTY_PATTERNS_HINT2));

  EditorEncodingCombo->Items->Add(DefaultEncodingName());
  EditorEncodingCombo->Items->Add(LoadStr(UTF8_NAME));

  std::unique_ptr<TStrings> Workspaces(StoredSessions->GetWorkspaces());
  AutoWorkspaceCombo->Items->AddStrings(Workspaces.get());
  AutoSaveWorkspacePasswordsCheck->Caption = LoadStr(SAVE_WORKSPACE_PASSWORDS);

  PuttyRegistryStorageKeyEdit->Items->Add(OriginalPuttyRegistryStorageKey);
  PuttyRegistryStorageKeyEdit->Items->Add(KittyRegistryStorageKey);

  MenuButton(RegisterAsUrlHandlersButton);
  MenuButton(EditorFontColorButton);
  MenuButton(EditorBackgroundColorButton);

  LoadDialogImage(CommanderInterfacePicture, L"Commander");
  LoadDialogImage(ExplorerInterfacePicture, L"Explorer");

  LinkLabel(UpdatesLink);
  LinkAppLabel(BackgroundConfirmationsLink);

  HideComponentsPanel(this);
}
//---------------------------------------------------------------------------
__fastcall TPreferencesDialog::~TPreferencesDialog()
{
  SAFE_DESTROY(FEditorScrollOnDragOver);
  SAFE_DESTROY(FCopyParamScrollOnDragOver);
  SAFE_DESTROY(FCustomCommandsScrollOnDragOver);
  delete FCustomCommandList;
  FCustomCommandList = NULL;
  delete FExtensionList;
  FExtensionList = NULL;
  delete FCopyParamList;
  FCopyParamList = NULL;
  delete FEditorList;
  FEditorList = NULL;
}
//---------------------------------------------------------------------
bool __fastcall TPreferencesDialog::Execute(TPreferencesDialogData * DialogData)
{
  PuttyPathEdit->Items = CustomWinConfiguration->History[L"PuttyPath"];
  FDialogData = DialogData;
  bool Result = (ShowModal() == DefaultResult(this));
  if (Result)
  {
    SaveConfiguration();
    CustomWinConfiguration->History[L"PuttyPath"] = PuttyPathEdit->Items;
  }
  else
  {
    for (int Index = 0; Index < FAddedExtensions->Count; Index++)
    {
      DeleteFile(ApiPath(FAddedExtensions->Strings[Index]));
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::LoadLanguages()
{
  if (!FLanguagesLoaded)
  {
    LanguagesView->Items->Clear();

    TStrings * Locales = GUIConfiguration->Locales;
    for (int Index = 0; Index < Locales->Count; Index++)
    {
      TListItem * Item = LanguagesView->Items->Add();
      Item->Caption = Locales->Strings[Index];
      Item->Data = Locales->Objects[Index];
      Item->Focused =
        (reinterpret_cast<LCID>(Locales->Objects[Index]) == GUIConfiguration->Locale);
      Item->Selected = Item->Focused;
    }

    FLanguagesLoaded = false;
  }
}
//---------------------------------------------------------------------------
TTabSheet * __fastcall TPreferencesDialog::FindPageForTreeNode(TTreeNode * Node)
{
  for (int pi = 0; pi < PageControl->PageCount; pi++)
  {
    TTabSheet * Sheet = PageControl->Pages[pi];
    if (Sheet->Tag == Node->SelectedIndex)
    {
      return Sheet;
    }
  }
  DebugFail();
  return NULL;
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::PrepareNavigationTree(TTreeView * Tree)
{
  Tree->FullExpand();
  int i = 0;
  while (i < Tree->Items->Count)
  {
    TTreeNode * Node = Tree->Items->Item[i];
    TTabSheet * Sheet = FindPageForTreeNode(Node);
    if (DebugNotNull(Sheet))
    {
      if (Sheet->Enabled)
      {
        // gets called multiple times occasionally
        // (e.g. when called from upload dialog invoked by /upload)
        if (!Sheet->Caption.IsEmpty())
        {
          Node->Text = Sheet->Caption;
          Sheet->Caption = L"";
        }
      }
      else
      {
        Tree->Items->Delete(Node);
        i--;
      }
    }
    i++;
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::LoadConfiguration()
{
  FNoUpdate++;
  try
  {
    #define BOOLPROP(PROP) PROP ## Check->Checked = WinConfiguration->PROP;
    BOOLPROP(DefaultDirIsHome);
    BOOLPROP(PreservePanelState);
    BOOLPROP(DeleteToRecycleBin);
    BOOLPROP(DDTransferConfirmation);
    BOOLPROP(DDWarnLackOfTempSpace);
    BOOLPROP(ShowHiddenFiles);
    BOOLPROP(RenameWholeName);
    BOOLPROP(ShowInaccesibleDirectories);
    BOOLPROP(CopyOnDoubleClickConfirmation);
    BOOLPROP(ConfirmTransferring);
    BOOLPROP(ConfirmOverwriting);
    BOOLPROP(ConfirmResume);
    BOOLPROP(ConfirmDeleting);
    BOOLPROP(ConfirmRecycling);
    BOOLPROP(ConfirmExitOnCompletion);
    BOOLPROP(ConfirmCommandSession);
    BOOLPROP(ContinueOnError);
    BOOLPROP(DDAllowMoveInit);
    BOOLPROP(BeepOnFinish);
    BOOLPROP(TemporaryDirectoryAppendSession);
    BOOLPROP(TemporaryDirectoryAppendPath);
    BOOLPROP(TemporaryDirectoryDeterministic);
    BOOLPROP(TemporaryDirectoryCleanup);
    BOOLPROP(ConfirmTemporaryDirectoryCleanup);
    BOOLPROP(FullRowSelect);

    ConfirmClosingSessionCheck2->Checked = WinConfiguration->ConfirmClosingSession;

    if (WinConfiguration->DDTransferConfirmation == asAuto)
    {
      // allow grayed state only initially,
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

    // select none when stNul
    RegistryStorageButton->Checked = (Configuration->Storage == stRegistry);
    IniFileStorageButton2->Checked = (Configuration->Storage == stIniFile);

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
    TWinConfiguration::RestoreFont(WinConfiguration->Editor.Font, FEditorFont.get());
    FEditorFont->Color = WinConfiguration->Editor.FontColor;
    FEditorBackgroundColor = WinConfiguration->Editor.BackgroundColor;
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

    GeneralSheet->Enabled = WinConfiguration->ExpertMode;
    ExplorerSheet->Enabled = WinConfiguration->ExpertMode;
    CommanderSheet->Enabled = WinConfiguration->ExpertMode;
    EditorSheet->Enabled = WinConfiguration->ExpertMode && !WinConfiguration->DisableOpenEdit;

    StorageGroup->Visible = WinConfiguration->ExpertMode;
    RandomSeedFileLabel->Visible = WinConfiguration->ExpertMode;
    RandomSeedFileEdit->Visible = WinConfiguration->ExpertMode;

    FCustomCommandList->Assign(WinConfiguration->CustomCommandList);
    FExtensionList->Assign(WinConfiguration->ExtensionList);
    UpdateCustomCommandsView();
    FCustomCommandOptions->Assign(WinConfiguration->CustomCommandOptions);

    PuttyPathEdit->Text = GUIConfiguration->PuttyPath;
    PuttyPasswordCheck2->Checked = GUIConfiguration->PuttyPassword;
    AutoOpenInPuttyCheck->Checked = WinConfiguration->AutoOpenInPutty;
    TelnetForFtpInPuttyCheck->Checked = WinConfiguration->TelnetForFtpInPutty;
    SelectPuttyRegistryStorageKey(GUIConfiguration->PuttyRegistryStorageKey);

    // Queue
    QueueTransferLimitEdit->AsInteger = GUIConfiguration->QueueTransfersLimit;
    EnableQueueByDefaultCheck->Checked = WinConfiguration->EnableQueueByDefault;
    QueueAutoPopupCheck->Checked = GUIConfiguration->QueueAutoPopup;
    QueueCheck->Checked = GUIConfiguration->DefaultCopyParam.Queue;
    QueueIndividuallyCheck->Checked = GUIConfiguration->DefaultCopyParam.QueueIndividually;
    QueueNoConfirmationCheck->Checked = GUIConfiguration->DefaultCopyParam.QueueNoConfirmation;
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
      DefaultStr(WinConfiguration->AutoWorkspace,
        // It will rarely happen that LastWorkspace is set, while AutoWorkspace not.
        // It can happen only when user saved workspace before opening the Preferences
        // dialog for the first time
        DefaultStr(WinConfiguration->LastWorkspace, LoadStr(NEW_WORKSPACE)));
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
    BOOLPROP(ExternalSessionInExistingInstance);
    BOOLPROP(KeepOpenWhenNoSession);
    BOOLPROP(ShowTips);

    // panels
    DoubleClickActionCombo->ItemIndex = WinConfiguration->DoubleClickAction;
    BOOLPROP(AutoReadDirectoryAfterOp);
    BOOLPROP(RefreshRemotePanel);
    RefreshRemotePanelIntervalEdit->Value =
      int(static_cast<double>(WinConfiguration->RefreshRemotePanelInterval) * SecsPerDay);

    switch (WinConfiguration->FormatSizeBytes)
    {
      case fbNone:
        FormatSizeBytesCombo->ItemIndex = 0;
        break;
      case fbKilobytes:
        FormatSizeBytesCombo->ItemIndex = 1;
        break;
      case fbShort:
        FormatSizeBytesCombo->ItemIndex = 2;
        break;
      default:
        DebugFail();
    }

    bool CustomPanelFont = !WinConfiguration->PanelFont.FontName.IsEmpty();
    PanelFontCheck->Checked = CustomPanelFont;
    if (CustomPanelFont)
    {
      TWinConfiguration::RestoreFont(WinConfiguration->PanelFont, FPanelFont.get());
    }
    else
    {
      FPanelFont->Assign(WinConfiguration->SystemIconFont);
    }

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

    UpdatesShowOnStartup->Checked = Updates.ShowOnStartup;
    UpdatesAuthenticationEmailEdit->Text = Updates.AuthenticationEmail;
    FVerifiedUpdatesAuthenticationEmail = UpdatesAuthenticationEmailEdit->Text;

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
    switch (CustomWinConfiguration->Interface)
    {
      case ifCommander:
        CommanderInterfaceButton2->Checked = true;
        break;

      case ifExplorer:
        ExplorerInterfaceButton2->Checked = true;
        break;

      default:
        DebugFail();
        break;
    }

    // security
    UseMasterPasswordCheck->Checked = WinConfiguration->UseMasterPassword;
    SessionRememberPasswordCheck->Checked = GUIConfiguration->SessionRememberPassword;

    // network
    RetrieveExternalIpAddressButton->Checked = Configuration->ExternalIpAddress.IsEmpty();
    CustomExternalIpAddressButton->Checked = !RetrieveExternalIpAddressButton->Checked;
    CustomExternalIpAddressEdit->Text = Configuration->ExternalIpAddress;
    TryFtpWhenSshFailsCheck->Checked = Configuration->TryFtpWhenSshFails;

    // logging
    EnableLoggingCheck->Checked = Configuration->Logging;
    LogProtocolCombo->ItemIndex = Configuration->LogProtocol;
    LogToFileCheck->Checked = Configuration->LogToFile;
    LogFileNameEdit3->Text =
      !Configuration->LogFileName.IsEmpty() ? Configuration->LogFileName : Configuration->DefaultLogFileName;
    if (Configuration->LogFileAppend)
    {
      LogFileAppendButton->Checked = true;
    }
    else
    {
      LogFileOverwriteButton->Checked = true;
    }
    BOOLPROP(LogSensitive);

    EnableActionsLoggingCheck->Checked = Configuration->LogActions;
    ActionsLogFileNameEdit->Text = Configuration->ActionsLogFileName;

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

    #define BOOLPROP(PROP) WinConfiguration->PROP = PROP ## Check->Checked
    BOOLPROP(DefaultDirIsHome);
    BOOLPROP(PreservePanelState);
    BOOLPROP(DeleteToRecycleBin);
    BOOLPROP(DDWarnLackOfTempSpace);
    BOOLPROP(ShowHiddenFiles);
    BOOLPROP(RenameWholeName);
    BOOLPROP(ShowInaccesibleDirectories);
    BOOLPROP(CopyOnDoubleClickConfirmation);
    BOOLPROP(ConfirmTransferring);
    BOOLPROP(ConfirmOverwriting);
    BOOLPROP(ConfirmResume);
    BOOLPROP(ConfirmDeleting);
    BOOLPROP(ConfirmRecycling);
    BOOLPROP(ConfirmExitOnCompletion);
    BOOLPROP(ConfirmCommandSession);
    BOOLPROP(ContinueOnError);
    BOOLPROP(DDAllowMoveInit);
    BOOLPROP(BeepOnFinish);
    BOOLPROP(TemporaryDirectoryAppendSession);
    BOOLPROP(TemporaryDirectoryAppendPath);
    BOOLPROP(TemporaryDirectoryDeterministic);
    BOOLPROP(TemporaryDirectoryCleanup);
    BOOLPROP(ConfirmTemporaryDirectoryCleanup);
    BOOLPROP(FullRowSelect);

    WinConfiguration->ConfirmClosingSession = ConfirmClosingSessionCheck2->Checked;

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

    ScpCommander.CompareByTime = CompareByTimeCheck->Checked;
    ScpCommander.CompareBySize = CompareBySizeCheck->Checked;

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
    TWinConfiguration::StoreFont(FEditorFont.get(), WinConfiguration->Editor.Font);
    WinConfiguration->Editor.FontColor = FEditorFont->Color;
    WinConfiguration->Editor.BackgroundColor = FEditorBackgroundColor;
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
    WinConfiguration->ExtensionList = FExtensionList;
    WinConfiguration->CustomCommandOptions = FCustomCommandOptions.get();

    GUIConfiguration->PuttyPath = PuttyPathEdit->Text;
    GUIConfiguration->PuttyPassword = PuttyPasswordCheck2->Checked;
    WinConfiguration->AutoOpenInPutty = AutoOpenInPuttyCheck->Checked;
    WinConfiguration->TelnetForFtpInPutty = TelnetForFtpInPuttyCheck->Checked;
    // do not overwrite custom keys
    if (PuttyRegistryStorageKeyEdit->ItemIndex >= 0)
    {
      GUIConfiguration->PuttyRegistryStorageKey = PuttyRegistryStorageKeyEdit->Text;
    }

    // Queue
    GUIConfiguration->QueueTransfersLimit = QueueTransferLimitEdit->AsInteger;
    WinConfiguration->EnableQueueByDefault = EnableQueueByDefaultCheck->Checked;
    GUIConfiguration->QueueAutoPopup = QueueAutoPopupCheck->Checked;
    CopyParam.Queue = QueueCheck->Checked;
    CopyParam.QueueIndividually = QueueIndividuallyCheck->Checked;
    CopyParam.QueueNoConfirmation = QueueNoConfirmationCheck->Checked;
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
    BOOLPROP(ExternalSessionInExistingInstance);
    BOOLPROP(KeepOpenWhenNoSession);
    BOOLPROP(ShowTips);

    // panels
    WinConfiguration->DoubleClickAction = (TDoubleClickAction)DoubleClickActionCombo->ItemIndex;
    BOOLPROP(AutoReadDirectoryAfterOp);
    BOOLPROP(RefreshRemotePanel);
    WinConfiguration->RefreshRemotePanelInterval =
      static_cast<double>(RefreshRemotePanelIntervalEdit->Value / SecsPerDay);

    switch (FormatSizeBytesCombo->ItemIndex)
    {
      case 0:
        WinConfiguration->FormatSizeBytes = fbNone;
        break;
      case 1:
        WinConfiguration->FormatSizeBytes = fbKilobytes;
        break;
      case 2:
        WinConfiguration->FormatSizeBytes = fbShort;
        break;
      default:
        DebugFail();
    }

    TFontConfiguration PanelFontConfiguration;
    if (PanelFontCheck->Checked)
    {
      TWinConfiguration::StoreFont(FPanelFont.get(), PanelFontConfiguration);
    }
    WinConfiguration->PanelFont = PanelFontConfiguration;

    // updates
    WinConfiguration->Updates = SaveUpdates();

    Configuration->CollectUsage = CollectUsageCheck->Checked;

    // presets
    WinConfiguration->CopyParamList = FCopyParamList;
    BOOLPROP(CopyParamAutoSelectNotice);

    // interface
    if (GetInterface() != CustomWinConfiguration->Interface)
    {
      Configuration->Usage->Inc(L"InterfaceChanges");
    }
    CustomWinConfiguration->Interface = GetInterface();

    // network
    Configuration->ExternalIpAddress =
      (CustomExternalIpAddressButton->Checked ? CustomExternalIpAddressEdit->Text : UnicodeString());
    Configuration->TryFtpWhenSshFails = TryFtpWhenSshFailsCheck->Checked;

    // security
    GUIConfiguration->SessionRememberPassword = SessionRememberPasswordCheck->Checked;

    // logging
    Configuration->Logging = EnableLoggingCheck->Checked;
    Configuration->LogProtocol = LogProtocolCombo->ItemIndex;
    Configuration->LogFileName = LogToFileCheck->Checked ? LogFileNameEdit3->Text : UnicodeString();
    Configuration->LogFileAppend = LogFileAppendButton->Checked;
    BOOLPROP(LogSensitive);

    Configuration->LogActions = EnableActionsLoggingCheck->Checked;
    Configuration->ActionsLogFileName = ActionsLogFileNameEdit->Text;

    // languages
    // As this dialog does not explicitly support run-time locale changing,
    // make this last, otherwise we lose some settings (or even worse
    // we end-up saving default text box values=control names as our configuration)
    if (LanguagesView->ItemFocused != NULL)
    {
      GUIConfiguration->Locale =
        reinterpret_cast<LCID>(LanguagesView->ItemFocused->Data);
    }

    // This possibly fails, make it last, so that the other settings are preserved.
    // Do nothing when no option is selected (i.e. storage is stNul).
    if (RegistryStorageButton->Checked)
    {
      Configuration->Storage = stRegistry;
    }
    else if (IniFileStorageButton2->Checked)
    {
      Configuration->Storage = stIniFile;
    }

    #undef BOOLPROP
  }
  __finally
  {
    Configuration->EndUpdate();
  }
}
//---------------------------------------------------------------------------
TUpdatesConfiguration __fastcall TPreferencesDialog::SaveUpdates()
{
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

  Updates.ShowOnStartup = UpdatesShowOnStartup->Checked;
  Updates.AuthenticationEmail = UpdatesAuthenticationEmailEdit->Text;

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

  return Updates;
}
//---------------------------------------------------------------------------
TInterface __fastcall TPreferencesDialog::GetInterface()
{
  return CommanderInterfaceButton2->Checked ? ifCommander : ifExplorer;
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::FormShow(TObject * /*Sender*/)
{
  // Load only now, particularly so that we have handles allocated already.
  // If called before ShowModal, we end up recreating the handles,
  // what causes flicker of the currently focused window
  LoadConfiguration();

  InstallPathWordBreakProc(RandomSeedFileEdit);
  InstallPathWordBreakProc(DDTemporaryDirectoryEdit);
  InstallPathWordBreakProc(PuttyPathEdit);
  InstallPathWordBreakProc(LogFileNameEdit3);
  InstallPathWordBreakProc(ActionsLogFileNameEdit);

  ListView_SetExtendedListViewStyle(CustomCommandsView->Handle, (ListView_GetExtendedListViewStyle(CustomCommandsView->Handle) & ~LVS_EX_INFOTIP));

  PrepareNavigationTree(NavigationTree);

  switch (FPreferencesMode) {
    case pmEditor: PageControl->ActivePage = EditorSheet; break;
    case pmCustomCommands: PageControl->ActivePage = CustomCommandsSheet; break;
    case pmQueue: PageControl->ActivePage = QueueSheet; break;
    case pmLogging: PageControl->ActivePage = LogSheet; break;
    case pmUpdates: PageControl->ActivePage = UpdatesSheet; break;
    case pmPresets: PageControl->ActivePage = CopyParamListSheet; break;
    case pmEditors: PageControl->ActivePage = EditorSheet; break;
    case pmCommander: PageControl->ActivePage = CommanderSheet; break;
    case pmEditorInternal: PageControl->ActivePage = EditorInternalSheet; break;
    default: PageControl->ActivePage = PreferencesSheet; break;
  }
  PageControlChange(NULL);
  ActiveControl = NavigationTree;
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
TCustomCommandList * __fastcall TPreferencesDialog::GetCommandList(int Index)
{
  if (Index < FCustomCommandList->Count)
  {
    return FCustomCommandList;
  }
  else
  {
    return FExtensionList;
  }
}
//---------------------------------------------------------------------------
int __fastcall TPreferencesDialog::GetCommandIndex(int Index)
{
  if (Index >= FCustomCommandList->Count)
  {
    Index -= FCustomCommandList->Count;
  }
  return Index;
}
//---------------------------------------------------------------------------
int __fastcall TPreferencesDialog::GetListCommandIndex(TCustomCommandList * List)
{
  int Index;
  if (GetCommandList(CustomCommandsView->ItemIndex) == List)
  {
    Index = GetCommandIndex(CustomCommandsView->ItemIndex);
  }
  else
  {
    Index = -1;
  }
  return Index;
}
//---------------------------------------------------------------------------
int __fastcall TPreferencesDialog::GetCommandListIndex(TCustomCommandList * List, int Index)
{
  if (List == FExtensionList)
  {
    Index += FCustomCommandList->Count;
  }
  return Index;
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::UpdateControls()
{
  if (FNoUpdate == 0)
  {
    EnableControl(BeepOnFinishAfterEdit, BeepOnFinishCheck->Checked);
    EnableControl(BeepOnFinishAfterText, BeepOnFinishCheck->Checked);

    EnableControl(ResumeThresholdEdit, ResumeSmartButton->Checked);
    EnableControl(ResumeThresholdUnitLabel2, ResumeThresholdEdit->Enabled);
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

    TFont * ActualPanelFont = PanelFontCheck->Checked ? FPanelFont.get() : WinConfiguration->SystemIconFont;
    UnicodeString PanelFontLabelText;
    PanelFontLabelText = FMTLOAD(EDITOR_FONT_FMT,
      (ActualPanelFont->Name, ActualPanelFont->Size));
    PanelFontLabel->Caption = PanelFontLabelText;
    if (!SameFont(PanelFontLabel->Font, ActualPanelFont))
    {
      PanelFontLabel->Font = ActualPanelFont;
    }

    EnableControl(RefreshRemotePanelIntervalEdit, RefreshRemotePanelCheck->Checked);
    EnableControl(RefreshRemoteDirectoryUnitLabel, RefreshRemotePanelCheck->Checked);

    UnicodeString EditorFontLabelText;
    EditorFontLabelText = FMTLOAD(EDITOR_FONT_FMT,
      (FEditorFont->Name, FEditorFont->Size)) + L"\n\n";
    EditorFontLabelText += TabSample(L"ABCD") + L"\n";
    EditorFontLabelText += TabSample(L"1234");
    EditorFontLabel->Caption = EditorFontLabelText;
    TColor EditorFontColor = GetWindowTextColor(FEditorFont->Color);
    if (!SameFont(EditorFontLabel->Font, FEditorFont.get()) ||
        (EditorFontLabel->Font->Color != EditorFontColor))
    {
      std::unique_ptr<TFont> Font(new TFont);
      Font->Assign(FEditorFont.get());
      Font->Color = EditorFontColor;
      EditorFontLabel->Font = Font.get();
    }
    EditorFontLabel->Color = GetWindowColor(FEditorBackgroundColor);

    TCustomCommandList * CommandList = GetCommandList(CustomCommandsView->ItemIndex);
    int CommandIndex = GetCommandIndex(CustomCommandsView->ItemIndex);
    bool CommandSelected = (CustomCommandsView->Selected != NULL);
    bool CustomCommandSelected = CommandSelected && (CommandList == FCustomCommandList);
    bool ExtensionSelected = CommandSelected && (CommandList == FExtensionList);
    EnableControl(EditCommandButton, CustomCommandSelected);
    EditCommandButton->Visible = !ExtensionSelected;
    EnableControl(ConfigureCommandButton, ExtensionSelected && CommandList->Commands[CommandIndex]->AnyOptionWithFlag(TCustomCommandType::ofConfig));
    ConfigureCommandButton->Visible = ExtensionSelected;
    EnableControl(RemoveCommandButton, CommandSelected);
    EnableControl(UpCommandButton, CommandSelected && (CommandIndex > 0));
    EnableControl(DownCommandButton, CommandSelected && (CommandIndex < CommandList->Count - 1));

    bool CopyParamSelected = (CopyParamListView->Selected != NULL);
    EnableControl(EditCopyParamButton, CopyParamSelected);
    EnableControl(DuplicateCopyParamButton, CopyParamSelected);
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
    SetLabelHintPopup(CopyParamLabel, InfoStr);

    EnableControl(DDExtEnabledButton, WinConfiguration->DDExtInstalled);
    EnableControl(DDExtEnabledLabel, WinConfiguration->DDExtInstalled);
    EnableControl(DDExtDisabledPanel, DDExtDisabledButton->Checked);
    EnableControl(DDTemporaryDirectoryEdit, DDCustomTemporaryDirectoryButton->Enabled &&
      DDCustomTemporaryDirectoryButton->Checked);
    EnableControl(DDWarnOnMoveCheck, DDExtDisabledButton->Checked &&
      DDAllowMoveInitCheck->Checked);
    EnableControl(ConfirmTemporaryDirectoryCleanupCheck,
      TemporaryDirectoryCleanupCheck->Checked);
    // allow only when some of the known storages is selected,
    // and particularly do not allow switching storage, when we start with stNul,
    // as that would destroy the stored configuration
    EnableControl(StorageGroup, RegistryStorageButton->Checked || IniFileStorageButton2->Checked);
    IniFileStorageButton2->Caption =
      AnsiReplaceStr(IniFileStorageButton2->Caption, L"(winscp.ini)",
        FORMAT(L"(%s)", (ExpandEnvironmentVariables(Configuration->IniFileStorageName))));

    EditorFontLabel->WordWrap = EditorWordWrapCheck->Checked;
    bool EditorSelected = (EditorListView3->Selected != NULL);
    EnableControl(EditEditorButton, EditorSelected);
    EnableControl(RemoveEditorButton, EditorSelected);
    EnableControl(UpEditorButton, EditorSelected &&
      (EditorListView3->ItemIndex > 0));
    EnableControl(DownEditorButton, EditorSelected &&
      (EditorListView3->ItemIndex < EditorListView3->Items->Count - 1));

    // updates
    EnableControl(UpdatesAuthenticationEmailEdit, FAutomaticUpdatesPossible);
    EnableControl(UpdatesAuthenticationEmailLabel, UpdatesAuthenticationEmailEdit->Enabled);
    EnableControl(UsageViewButton, CollectUsageCheck->Checked);
    EnableControl(UpdatesProxyHostEdit, UpdatesProxyCheck->Checked);
    EnableControl(UpdatesProxyHostLabel, UpdatesProxyHostEdit->Enabled);
    EnableControl(UpdatesProxyPortEdit, UpdatesProxyCheck->Checked);
    EnableControl(UpdatesProxyPortLabel, UpdatesProxyPortEdit->Enabled);

    bool IsSiteCommand = false;
    bool IsPasswordCommand = false;
    try
    {
      TRemoteCustomCommand RemoteCustomCommand;
      TInteractiveCustomCommand InteractiveCustomCommand(&RemoteCustomCommand);
      UnicodeString PuttyPath = PuttyPathEdit->Text;
      PuttyPath = InteractiveCustomCommand.Complete(PuttyPath, false);
      IsSiteCommand = RemoteCustomCommand.IsSiteCommand(PuttyPath);
      IsPasswordCommand = RemoteCustomCommand.IsPasswordCommand(PuttyPath);
    }
    catch (...)
    {
      // noop
    }
    bool AnyPuttyPath = !PuttyPathEdit->Text.IsEmpty();
    EnableControl(PuttyPasswordCheck2, AnyPuttyPath && !IsPasswordCommand);
    EnableControl(AutoOpenInPuttyCheck, AnyPuttyPath);
    EnableControl(TelnetForFtpInPuttyCheck,
      AnyPuttyPath && !IsSiteCommand);
    EnableControl(PuttyRegistryStorageKeyEdit,
      AnyPuttyPath && !IsSiteCommand);
    EnableControl(PuttyRegistryStorageKeyLabel, PuttyRegistryStorageKeyEdit->Enabled);

    EnableControl(SetMasterPasswordButton, WinConfiguration->UseMasterPassword);

    // network
    EnableControl(CustomExternalIpAddressEdit, CustomExternalIpAddressButton->Checked);

    // window
    EnableControl(AutoWorkspaceCombo, AutoSaveWorkspaceCheck->Checked);
    EnableControl(AutoWorkspaceLabel, AutoWorkspaceCombo->Enabled);
    EnableControl(AutoSaveWorkspacePasswordsCheck,
      !Configuration->DisablePasswordStoring &&
      AutoWorkspaceCombo->Enabled);

    // integration
    // There's no quick launch in Windows 7
    EnableControl(QuickLaunchIconButton, !IsWin7());
    MakeDefaultHandlerItem->Visible = IsWinVista();

    // languages
    LanguageChangeLabel->Visible =
      DebugAlwaysTrue(!GUIConfiguration->CanApplyLocaleImmediately) &&
      (LanguagesView->ItemFocused != NULL) &&
      (reinterpret_cast<LCID>(LanguagesView->ItemFocused->Data) != GUIConfiguration->AppliedLocale);

    // logging
    EnableControl(LogProtocolCombo, EnableLoggingCheck->Checked);
    EnableControl(LogToFileCheck, LogProtocolCombo->Enabled);
    EnableControl(LogFileNameEdit3, LogToFileCheck->Enabled && LogToFileCheck->Checked);
    EnableControl(LogFileNameHintText, LogFileNameEdit3->Enabled);
    EnableControl(LogFileAppendButton, LogFileNameEdit3->Enabled);
    EnableControl(LogFileOverwriteButton, LogFileNameEdit3->Enabled);

    EnableControl(LogSensitiveCheck, LogProtocolCombo->Enabled);

    EnableControl(ActionsLogFileNameEdit, EnableActionsLoggingCheck->Checked);
    EnableControl(ActionsLogFileNameHintText, ActionsLogFileNameEdit->Enabled);

    // interface
    InterfaceChangeLabel->Visible =
      !CustomWinConfiguration->CanApplyInterfaceImmediately &&
      (GetInterface() != CustomWinConfiguration->AppliedInterface);
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::EditorFontButtonClick(TObject * /*Sender*/)
{
  if (FontDialog(FEditorFont.get()))
  {
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::EditorFontColorChange(TColor Color)
{
  FEditorFont->Color = Color;
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::EditorFontColorButtonClick(TObject * /*Sender*/)
{
  // WORKAROUND: Compiler keeps crashing randomly (but frequently) with
  // "internal error" when passing menu directly to unique_ptr.
  // Splitting it to two statements seems to help.
  // The same hack exists in TSiteAdvancedDialog::ColorButtonClick
  TPopupMenu * Menu = CreateColorPopupMenu(FEditorFont->Color, EditorFontColorChange);
  // Popup menu has to survive the popup as TBX calls click handler asynchronously (post).
  FColorPopupMenu.reset(Menu);
  MenuPopup(Menu, EditorFontColorButton);
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::EditorBackgroundColorChange(TColor Color)
{
  FEditorBackgroundColor = Color;
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::EditorBackgroundColorButtonClick(TObject * /*Sender*/)
{
  // See comment in EditorFontColorButtonClick.
  // We are using session color (contrary to editor text color) for background
  // for a consistency with color selection menu on editor toolbar.
  TTBXPopupMenu * PopupMenu = new TTBXPopupMenu(Application);
  FColorPopupMenu.reset(PopupMenu);
  CreateEditorBackgroundColorMenu(PopupMenu->Items, FEditorBackgroundColor, EditorBackgroundColorChange);
  MenuPopup(FColorPopupMenu.get(), EditorBackgroundColorButton);
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::PanelFontButtonClick(TObject * /*Sender*/)
{
  if (FontDialog(FPanelFont.get()))
  {
    PanelFontCheck->Checked = true;
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::FormCloseQuery(TObject * /*Sender*/,
  bool & /*CanClose*/)
{
  if (ModalResult == DefaultResult(this))
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
      MessageDialog(LoadStr(CREATE_DESKTOP_ICON2), qtConfirmation,
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
    if (MessageDialog(MainInstructions(LoadStr(CONFIRM_CREATE_ICON)),
          qtConfirmation, qaYes | qaNo, HELP_CREATE_ICON) == qaYes)
    {
      if (Sender == SendToHookButton)
      {
        IconName = FMTLOAD(SENDTO_HOOK_NAME2, (AppName));
        SpecialFolder = CSIDL_SENDTO;
        Params = TProgramParams::FormatSwitch(UPLOAD_SWITCH);
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

  TInstantOperationVisualizer Visualizer;

  CreateDesktopShortCut(IconName,
    Application->ExeName, Params, L"", SpecialFolder);
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::CustomCommandsViewData(TObject * /*Sender*/,
      TListItem * Item)
{
  // WORKAROUND We get here on Wine after destructor is called
  if ((FCustomCommandList != NULL) && (FExtensionList != NULL))
  {
    int Index = Item->Index;
    const TCustomCommandType * Command = GetCommandList(Index)->Commands[GetCommandIndex(Index)];
    UnicodeString Caption = StripHotkey(Command->Name);
    Item->Caption = Caption;
    DebugAssert(!Item->SubItems->Count);
    Item->SubItems->Add(Command->Command);
    Item->SubItems->Add(LoadStr(
      FLAGSET(Command->Params, ccLocal) ? CUSTOM_COMMAND_LOCAL : CUSTOM_COMMAND_REMOTE));
  }
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
  CustomCommandsView->Items->Count = FCustomCommandList->Count + FExtensionList->Count;
  AutoSizeListColumnsWidth(CustomCommandsView, 1);
  CustomCommandsView->Invalidate();
  // particularly after command is edited/configured, make sure the hint is updated,
  // even if we manage to display a hint for the same command as before the change
  FCustomCommandsHintItem = NULL;
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
    AddEditCommand(false);
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::CustomCommandsViewDblClick(
  TObject * /*Sender*/)
{
  if (EditCommandButton->Enabled)
  {
    AddEditCommand(true);
  }
  else if (ConfigureCommandButton->Enabled)
  {
    ConfigureCommand();
  }
}
//---------------------------------------------------------------------------
static int __fastcall AddCommandToList(TCustomCommandList * List, int Index, TCustomCommandType * Command)
{
  if (Index >= 0)
  {
    List->Insert(Index, Command);
  }
  else
  {
    List->Add(Command);
    Index = List->Count - 1;
  }
  return Index;
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::AddEditCommand(bool Edit)
{
  TCustomCommandType Command;

  if (Edit)
  {
    int Index = CustomCommandsView->ItemIndex;
    DebugAssert(GetCommandList(Index) == FCustomCommandList);

    Command = *FCustomCommandList->Commands[GetCommandIndex(Index)];
  }

  TShortCuts ShortCuts;
  if (WinConfiguration->SharedBookmarks != NULL)
  {
    WinConfiguration->SharedBookmarks->ShortCuts(ShortCuts);
  }
  FCustomCommandList->ShortCuts(ShortCuts);
  FExtensionList->ShortCuts(ShortCuts);

  if (DoCustomCommandDialog(Command, FCustomCommandList,
        (Edit ? ccmEdit : ccmAdd), 0, NULL, &ShortCuts))
  {
    int Index = GetListCommandIndex(FCustomCommandList);
    TCustomCommandType * ACommand = new TCustomCommandType(Command);
    if (Edit)
    {
      DebugAssert(Index < FCustomCommandList->Count);
      FCustomCommandList->Change(Index, ACommand);
    }
    else
    {
      Index = AddCommandToList(FCustomCommandList, Index, ACommand);
    }

    UpdateCustomCommandsView();
    CustomCommandsView->ItemIndex = GetCommandListIndex(FCustomCommandList, Index);
    CustomCommandsView->ItemFocused->MakeVisible(false);
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::RemoveCommandButtonClick(
      TObject * /*Sender*/)
{
  TCustomCommandList * List = GetCommandList(CustomCommandsView->ItemIndex);
  int Index = GetCommandIndex(CustomCommandsView->ItemIndex);
  if (List == FExtensionList)
  {
    const TCustomCommandType * CustomComand = List->Commands[Index];

    // If the extension was added in this "preferences session", remove the file
    int PathIndex = FAddedExtensions->IndexOf(CustomComand->FileName);
    if (PathIndex >= 0)
    {
      FAddedExtensions->Delete(PathIndex);
      DeleteFile(ApiPath(CustomComand->FileName));
    }
  }
  List->Delete(Index);
  UpdateCustomCommandsView();
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::CustomCommandMove(int Source, int Dest)
{
  TCustomCommandList * List = GetCommandList(CustomCommandsView->ItemIndex);
  int SourceIndex = GetCommandIndex(Source);
  int DestIndex = GetCommandIndex(Dest);
  List->Move(SourceIndex, DestIndex);
  // workaround for bug in VCL
  CustomCommandsView->ItemIndex = -1;
  CustomCommandsView->ItemFocused = CustomCommandsView->Selected;
  CustomCommandsView->ItemIndex = Dest;
  UpdateCustomCommandsView();
  UpdateControls();
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
  else if (ListView == EditorListView3)
  {
    return FEditorScrollOnDragOver;
  }
  else
  {
    DebugFail();
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
static int __fastcall PointToListViewIndex(TObject * Sender, int X, int Y)
{
  TListItem * Item = dynamic_cast<TListView*>(Sender)->GetItemAt(X, Y);
  return Item ? Item->Index : -1;
}
//---------------------------------------------------------------------------
bool __fastcall TPreferencesDialog::AllowListViewDrag(TObject * Sender, int X, int Y)
{
  FListViewDragDest = PointToListViewIndex(Sender, X, Y);
  return (FListViewDragDest >= 0) && (FListViewDragDest != FListViewDragSource);
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::CustomCommandsViewDragDrop(
      TObject * Sender, TObject * Source, int X, int Y)
{
  if (Source == CustomCommandsView)
  {
    if (AllowListViewDrag(Sender, X, Y) &&
        (GetCommandList(FListViewDragSource) == GetCommandList(FListViewDragDest)))
    {
      CustomCommandMove(FListViewDragSource, FListViewDragDest);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::CustomCommandsViewDragOver(
  TObject *Sender, TObject *Source, int X, int Y, TDragState State, bool & Accept)
{
  ListViewDragOver(Sender, Source, X, Y, State, Accept);

  if (Source == Sender)
  {
    int Dest = PointToListViewIndex(Sender, X, Y);
    if (GetCommandList(FListViewDragSource) != GetCommandList(Dest))
    {
      Accept = false;
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
  DebugAssert(CopyParamListView->ItemIndex >= 1 &&
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
  if ((Index == 0) && (Mode == cpmEdit))
  {
    Result = DoCopyParamCustomDialog(FCopyParams, 0);
  }
  else
  {
    TCopyParamRuleData * CopyParamRuleData =
      (FDialogData != NULL ? FDialogData->CopyParamRuleData : NULL);
    // negative (when default is selected) means add to the end
    Index--;
    Result = DoCopyParamPresetDialog(FCopyParamList, Index, Mode, CopyParamRuleData, FCopyParams);
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
  AddEditCopyParam(cpmDuplicate);
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
    EditorListView3->ItemIndex = -1;
    EditorListView3->ItemFocused = EditorListView3->Selected;
    EditorListView3->ItemIndex = Dest;
    UpdateEditorListView();
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::EditorListView3DragDrop(TObject * Sender,
  TObject * Source, int X, int Y)
{
  if (Source == EditorListView3)
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
  EditorMove(EditorListView3->ItemIndex,
    EditorListView3->ItemIndex + (Sender == UpEditorButton ? -1 : 1));
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::RemoveEditorButtonClick(
  TObject * /*Sender*/)
{
  DebugAssert(EditorListView3->ItemIndex >= 0 &&
    EditorListView3->ItemIndex < FEditorList->Count);
  FEditorList->Delete(EditorListView3->ItemIndex);
  UpdateEditorListView();
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::AddEditEditorButtonClick(TObject * Sender)
{
  TEditorPreferencesMode Mode = (Sender == EditEditorButton ? epmEdit : epmAdd);
  int Index = EditorListView3->ItemIndex;
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
      EditorListView3->ItemIndex = Index;
      UpdateControls();
    }
  }
  __finally
  {
    delete Editor;
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::EditorListView3DblClick(TObject * /*Sender*/)
{
  if (EditEditorButton->Enabled)
  {
    AddEditEditorButtonClick(EditEditorButton);
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::EditorListView3KeyDown(TObject * /*Sender*/,
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
  EditorListView3->Items->Count = FEditorList->Count;
  AutoSizeListColumnsWidth(EditorListView3);
  EditorListView3->Invalidate();
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::EditorListView3Data(TObject * /*Sender*/,
  TListItem * Item)
{
  // WORKAROUND We get here on Wine after destructor is called
  if (FEditorList != NULL)
  {
    int Index = Item->Index;
    DebugAssert(Index >= 0 && Index <= FEditorList->Count);
    const TEditorPreferences * Editor = FEditorList->Editors[Index];
    Item->Caption = Editor->Name;
    Item->SubItems->Add(Editor->Data->FileMask.Masks);
    if (Editor->Data->Editor == edExternal)
    {
      Item->SubItems->Add(BooleanToStr(Editor->Data->ExternalEditorText));
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::NavigationTreeChange(TObject * /*Sender*/,
  TTreeNode * Node)
{
  if (DebugAlwaysTrue(Node->SelectedIndex > 0))
  {
    PageControl->ActivePage = DebugNotNull(FindPageForTreeNode(Node));
    // reshow the accelerators, etc
    ResetSystemSettings(this);
    // This is particularly here to enable EditCopyParamButton,
    // as for some reason CopyParamListView->Selected is NULL until
    // its page is shown for the first time
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::PageControlChange(TObject * /*Sender*/)
{
  // this is probably only ever called from FormShow (explicitly)
  bool Found = false;
  if (DebugAlwaysTrue(PageControl->ActivePage->Tag > 0))
  {
    for (int Index = 0; Index < NavigationTree->Items->Count; Index++)
    {
      if (NavigationTree->Items->Item[Index]->SelectedIndex ==
            PageControl->ActivePage->Tag)
      {
        NavigationTree->Items->Item[Index]->Selected = true;
        Found = true;
      }
    }
  }

  if (DebugAlwaysTrue(Found))
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
  DebugAssert(Message.HelpInfo != NULL);

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
  DebugAssert(M);
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
void __fastcall TPreferencesDialog::RegisterAsUrlHandlersButtonClick(
  TObject * /*Sender*/)
{
  MenuPopup(RegisterAsUrlHandlerMenu, RegisterAsUrlHandlersButton);
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::RegisterAsUrlHandlerItemClick(TObject * /*Sender*/)
{
  unsigned int Result =
    MessageDialog(MainInstructions(LoadStr(CONFIRM_REGISTER_URL2)),
      qtConfirmation, qaYes | qaNo, HELP_REGISTER_URL);
  if (Result == qaYes)
  {
    TInstantOperationVisualizer Visualizer;

    RegisterForDefaultProtocols();
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::UnregisterForDefaultProtocolsItemClick(TObject * /*Sender*/)
{
  unsigned int Result =
    MessageDialog(MainInstructions(LoadStr(CONFIRM_UNREGISTER_URL)),
      qtConfirmation, qaYes | qaNo, HELP_REGISTER_URL);
  if (Result == qaYes)
  {
    TInstantOperationVisualizer Visualizer;

    UnregisterForProtocols();
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::MakeDefaultHandlerItemClick(TObject * /*Sender*/)
{
  TOperationVisualizer Visualizer;
  LaunchAdvancedAssociationUI();
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
  if (MessageDialog(MainInstructions(FMTLOAD(CONFIRM_ADD_SEARCH_PATH, (AppPath))),
        qtConfirmation, qaYes | qaNo, HELP_ADD_SEARCH_PATH) == qaYes)
  {
    TInstantOperationVisualizer Visualizer;

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
  AutoSizeListColumnsWidth(CopyParamListView);
  CopyParamListView->Invalidate();
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::CopyParamListViewData(TObject * /*Sender*/,
  TListItem * Item)
{
  // WORKAROUND We get here on Wine after destructor is called
  if (FCopyParamList != NULL)
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
      DebugAssert(Index >= 1 && Index <= 1 + FCopyParamList->Count);
      Name = StripHotkey(FCopyParamList->Names[Index - 1]);
      Rule = BooleanToStr(FCopyParamList->Rules[Index - 1] != NULL);
    }

    Item->Caption = Name;
    Item->SubItems->Add(Rule);
  }
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
  UnicodeString Executables = FORMAT("%s;%s", (OriginalPuttyExecutable, KittyExecutable));
  BrowseForExecutable(PuttyPathEdit, LoadStr(PREFERENCES_SELECT_PUTTY2),
    FMTLOAD(PREFERENCES_PUTTY_FILTER2, (Executables)), false, false);
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
bool __fastcall TPreferencesDialog::CanSetMasterPassword()
{
  bool Result;
  bool Retry;
  do
  {
    Retry = false;
    Result = !AnyOtherInstanceOfSelf();

    if (!Result)
    {
      unsigned int Answer =
        MessageDialog(
          LoadStr(MASTER_PASSWORD_OTHER_INSTANCE),
          qtConfirmation, qaRetry | qaIgnore | qaCancel,
          HELP_MASTER_PASSWORD);

      switch (Answer)
      {
        case qaRetry:
          Retry = true;
          break;

        case qaIgnore:
          Result = true;
          break;

        case qaCancel:
        default:
          // noop
          break;
      }
    }
  }
  while (Retry && !Result);

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::MasterPasswordChanged(
  UnicodeString Message, TStrings * RecryptPasswordErrors)
{
  // Save master password.
  // This is unlikely to fail, as storage is written explicitly already
  // when writing the recrypted passwords
  Configuration->SaveExplicit();

  TQueryType QueryType = qtInformation;
  if (RecryptPasswordErrors->Count > 0)
  {
    Message = FMTLOAD(MASTER_PASSWORD_RECRYPT_ERRORS, (Message));
    QueryType = qtWarning;
  }
  MoreMessageDialog(
    Message, RecryptPasswordErrors, QueryType, qaOK, HELP_MASTER_PASSWORD);
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::ChangeMasterPassword(UnicodeString Message)
{
  UnicodeString NewPassword;
  if (DoChangeMasterPasswordDialog(NewPassword))
  {
    std::unique_ptr<TStrings> RecryptPasswordErrors(new TStringList());
    WinConfiguration->ChangeMasterPassword(NewPassword, RecryptPasswordErrors.get());
    MasterPasswordChanged(Message, RecryptPasswordErrors.get());
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
      if (CanSetMasterPassword())
      {
        if (UseMasterPasswordCheck->Checked)
        {
          ChangeMasterPassword(LoadStr(MASTER_PASSWORD_SET2));
        }
        else
        {
          if (DoMasterPasswordDialog())
          {
            std::unique_ptr<TStrings> RecryptPasswordErrors(new TStringList());
            WinConfiguration->ClearMasterPassword(RecryptPasswordErrors.get());
            MasterPasswordChanged(LoadStr(MASTER_PASSWORD_CLEARED2), RecryptPasswordErrors.get());
          }
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
  if (CanSetMasterPassword())
  {
    ChangeMasterPassword(MainInstructions(LoadStr(MASTER_PASSWORD_CHANGED)));
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::UsageViewButtonClick(TObject * /*Sender*/)
{
  std::unique_ptr<TStrings> Data(TextToStringList(GetUsageData()));
  UnicodeString Message =
    Data->Text.IsEmpty() ? MainInstructions(LoadStr(USAGE_DATA_NONE)) : LoadStr(USAGE_DATA2);
  MoreMessageDialog(Message, Data.get(), qtInformation, qaOK, HELP_USAGE);
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
void __fastcall TPreferencesDialog::SelectPuttyRegistryStorageKey(const UnicodeString & Key)
{
  PuttyRegistryStorageKeyEdit->ItemIndex =
    PuttyRegistryStorageKeyEdit->Items->IndexOf(Key);
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::PuttyPathEditChange(TObject * /*Sender*/)
{
  UnicodeString PuttyPath = PuttyPathEdit->Text;
  if (ContainsText(PuttyPath, OriginalPuttyExecutable))
  {
    SelectPuttyRegistryStorageKey(OriginalPuttyRegistryStorageKey);
  }
  else if (ContainsText(PuttyPath, KittyExecutable))
  {
    SelectPuttyRegistryStorageKey(KittyRegistryStorageKey);
  }

  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::NavigationTreeChanging(TObject * /*Sender*/,
  TTreeNode * Node, bool & /*AllowChange*/)
{
  TTabSheet * Sheet = FindPageForTreeNode(Node);
  // delay load as this can be time consuming
  if (Sheet == LanguagesSheet)
  {
    LoadLanguages();
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::LanguagesGetMoreButtonClick(TObject * /*Sender*/)
{
  OpenBrowser(ProgramUrl(LoadStr(LOCALES_URL)));
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::CommanderClick(TObject * /*Sender*/)
{
  CommanderInterfaceButton2->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::ExplorerClick(TObject * /*Sender*/)
{
  ExplorerInterfaceButton2->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::PanelFontLabelDblClick(TObject * Sender)
{
  PanelFontButtonClick(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::UpdatesAuthenticationEmailEditExit(TObject * /*Sender*/)
{
  if (FVerifiedUpdatesAuthenticationEmail != UpdatesAuthenticationEmailEdit->Text)
  {
    FVerifiedUpdatesAuthenticationEmail = UpdatesAuthenticationEmailEdit->Text;

    if (!FVerifiedUpdatesAuthenticationEmail.IsEmpty())
    {
      TUpdatesConfiguration Updates = SaveUpdates();

      {
        TInstantOperationVisualizer Visualizer;
        QueryUpdates(Updates);
      }

      UnicodeString AuthenticationError = Updates.Results.AuthenticationError;
      if (!AuthenticationError.IsEmpty())
      {
        AuthenticationError = FormatUpdatesMessage(AuthenticationError);
        if (HasParagraphs(AuthenticationError))
        {
          AuthenticationError = MainInstructionsFirstParagraph(AuthenticationError);
        }
        else
        {
          AuthenticationError = MainInstructions(AuthenticationError);
        }

        UnicodeString HelpUrl = GetEnableAutomaticUpdatesUrl();
        unsigned int Result =
          MoreMessageDialog(AuthenticationError, NULL, qtError, qaIgnore | qaAbort, HelpUrl);
        if (Result == qaAbort)
        {
          Abort();
        }
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::UpdatesLinkClick(TObject * /*Sender*/)
{
  EnableAutomaticUpdates();
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::CustomCommandsViewWindowProc(TMessage & Message)
{
  FOrigCustomCommandsViewWindowProc(Message);

  if (Message.Msg == CN_NOTIFY)
  {
    TWMNotify & NotifyMessage = reinterpret_cast<TWMNotify &>(Message);

    if (NotifyMessage.NMHdr->code == NM_CUSTOMDRAW)
    {
      // request CDDS_ITEMPOSTPAINT notification
      Message.Result |= CDRF_NOTIFYPOSTPAINT | CDRF_NOTIFYSUBITEMDRAW;

      TNMLVCustomDraw * CustomDraw = reinterpret_cast<TNMLVCustomDraw *>(NotifyMessage.NMHdr);
      int Index = CustomDraw->nmcd.dwItemSpec;
      int CommandIndex = GetCommandIndex(Index);
      TCustomCommandList * List = GetCommandList(Index);
      // after end of every list, except for the last last list
      if ((CommandIndex == List->Count - 1) && (Index < CustomCommandsView->Items->Count - 1) &&
          FLAGSET(CustomDraw->nmcd.dwDrawStage, CDDS_ITEMPOSTPAINT))
      {
        TRect Rect;
        Rect.Top = CustomDraw->iSubItem;
        Rect.Left = LVIR_BOUNDS;
        CustomCommandsView->Perform(LVM_GETSUBITEMRECT, CustomDraw->nmcd.dwItemSpec, reinterpret_cast<LPARAM>(&Rect));

        HDC DC = CustomDraw->nmcd.hdc;

        SelectObject(DC, GetStockObject(DC_PEN));
        SetDCPenColor(DC, ColorToRGB(clWindowFrame));

        MoveToEx(DC, Rect.Left, Rect.Bottom - 1, NULL);
        LineTo(DC, Rect.Right, Rect.Bottom - 1);
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::AddExtension()
{
  const UnicodeString HistoryKey(L"ExtensionPath");
  std::unique_ptr<TStrings> History(CloneStrings(CustomWinConfiguration->History[HistoryKey]));
  UnicodeString Path;
  if (InputDialog(LoadStr(ADD_EXTENSION_CAPTION), LoadStr(ADD_EXTENSION_PROMPT), Path,
        HELP_NONE, History.get(), true) &&
      !Path.IsEmpty())
  {
    CustomWinConfiguration->History[HistoryKey] = History.get();

    bool Trusted;
    bool Latest;
    UnicodeString FileName;
    UnicodeString ExtensionPath;
    std::unique_ptr<TStringList> Lines(new TStringList());
    std::unique_ptr<TCustomCommandType> CustomCommand;

    bool IsUrl = IsHttpOrHttpsUrl(Path);

    try
    {
      if (IsUrl)
      {
        UnicodeString Url = Path;
        Url = SecureUrl(Url);
        bool WinSCPURL = IsWinSCPUrl(Url);
        if (WinSCPURL)
        {
          Url = ProgramUrl(Url);
          // The EncodeUrlString should not be necessary, but as we get the value from regitry, let's be safe
          Url = AppendUrlParams(Url, FORMAT(L"netframework=%s", (EncodeUrlString(GetNetVersionStr()))));
          Url = AppendUrlParams(Url, FORMAT(L"powershell=%s", (EncodeUrlString(GetPowerShellVersionStr()))));
          Url = AppendUrlParams(Url, FORMAT(L"windows=%s", (EncodeUrlString(WindowsVersion()))));
          Url = CampaignUrl(Url);
        }

        TOperationVisualizer Visualizer;

        std::unique_ptr<THttp> Http(CreateHttp());
        Http->URL = Url;
        std::unique_ptr<TStrings> Headers(new TStringList());
        Headers->Values[L"Accept"] = L"text/winscpextension,text/plain";
        Http->RequestHeaders = Headers.get();
        Http->Get();

        UnicodeString TrustedStr = Http->ResponseHeaders->Values["WinSCP-Extension-Trusted"];
        Trusted = WinSCPURL && (StrToIntDef(TrustedStr, 0) != 0);

        FileName = MakeValidFileName(Http->ResponseHeaders->Values["WinSCP-Extension-Id"]);
        if (FileName.IsEmpty())
        {
          FileName = MakeValidFileName(ExtractFileNameFromUrl(Path));
        }
        Lines->Text = Http->Response;

        Latest = Http->ResponseHeaders->Values["WinSCP-Extension-Skipped"].Trim().IsEmpty();
      }
      else
      {
        if (!FileExists(ApiPath(Path)))
        {
          throw Exception(MainInstructions(FMTLOAD(FILE_NOT_EXISTS, (Path))));
        }

        Trusted = true;
        Latest = true;

        UnicodeString Id = WinConfiguration->GetExtensionId(Path);
        FileName = ExtractFileName(Path);
        if (!Id.IsEmpty())
        {
          ExtensionPath = Path;
        }

        LoadScriptFromFile(Path, Lines.get());
      }

      // validate syntax
      CustomCommand.reset(new TCustomCommandType());
      CustomCommand->LoadExtension(Lines.get(), FileName);
    }
    catch (Exception & E)
    {
      throw ExtException(&E, MainInstructions(FMTLOAD(EXTENSION_LOAD_ERROR, (Path))));
    }

    if (!ExtensionPath.IsEmpty())
    {
      int Index = FExtensionList->FindIndexByFileName(Path);
      if (Index > 0)
      {
        CustomCommandsView->ItemIndex = GetCommandListIndex(FExtensionList, Index);
        CustomCommandsView->ItemFocused->MakeVisible(false);
        CustomCommandsView->SetFocus();
        throw Exception(MainInstructions(LoadStr(EXTENSION_INSTALLED_ALREADY)));
      }
    }

    if (FExtensionList->Find(CustomCommand->Name) != NULL)
    {
      throw Exception(MainInstructions(FMTLOAD(EXTENSION_DUPLICATE, (StripHotkey(CustomCommand->Name)))));
    }

    if (ExtensionPath.IsEmpty())
    {
      if (TCustomCommandType::GetExtensionId(FileName).IsEmpty())
      {
        UnicodeString FileNameOnly = ExtractFileNameOnly(FileName);
        if (FileNameOnly.IsEmpty())
        {
          FileName = MakeValidFileName(StripHotkey(CustomCommand->Name)) + WinSCPExtensionExt;
        }
        else
        {
          FileName = ExtractFileNameOnly(FileName) + WinSCPExtensionExt + ExtractFileExt(FileName);
        }
      }
    }

    if (Trusted ||
        (MessageDialog(MainInstructions(LoadStr(EXTENSION_UNTRUSTED)), qtWarning, qaOK | qaCancel) == qaOK))
    {
      if (ExtensionPath.IsEmpty())
      {
        UnicodeString UserExtensionsPath = WinConfiguration->GetUserExtensionsPath();
        if (!DirectoryExists(UserExtensionsPath) &&
            !ForceDirectories(UserExtensionsPath))
        {
          throw EOSExtException(MainInstructions(FMTLOAD(CREATE_LOCAL_DIR_ERROR, (UserExtensionsPath))));
        }

        ExtensionPath = IncludeTrailingBackslash(UserExtensionsPath) + FileName;

        int Counter = 1;
        UnicodeString OriginalExtensionPath = ExtensionPath;
        int P = Pos(UpperCase(WinSCPExtensionExt), UpperCase(OriginalExtensionPath));

        while (FileExists(ApiPath(ExtensionPath)))
        {
          Counter++;
          ExtensionPath = LeftStr(OriginalExtensionPath, P - 1) + IntToStr(Counter) + RightStr(OriginalExtensionPath, OriginalExtensionPath.Length() - P + 1);
        }

        Lines->SaveToFile(ApiPath(ExtensionPath));

        FAddedExtensions->Add(ExtensionPath);
      }

      int Index = GetListCommandIndex(FExtensionList);

      std::unique_ptr<TCustomCommandType> CustomCommand(new TCustomCommandType());
      CustomCommand->Id = WinConfiguration->GetExtensionId(ExtensionPath);
      CustomCommand->LoadExtension(ExtensionPath);

      Index = AddCommandToList(FExtensionList, Index, CustomCommand.release());

      UpdateCustomCommandsView();
      CustomCommandsView->ItemIndex = GetCommandListIndex(FExtensionList, Index);
      CustomCommandsView->ItemFocused->MakeVisible(false);
      UpdateControls();

      if (!Latest)
      {
        MessageDialog(LoadStr(EXTENSION_NOT_LATEST), qtInformation, qaOK);
      }

      if (IsUrl)
      {
        Configuration->Usage->Inc(L"ExtensionAddsFromUrl");
      }
      else
      {
        Configuration->Usage->Inc(L"ExtensionAddsFromFile");
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::AddCommandButtonClick(TObject * /*Sender*/)
{
  if (GetCommandList(CustomCommandsView->ItemIndex) == FCustomCommandList)
  {
    AddEditCommand(false);
  }
  else
  {
    AddExtension();
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::AddCustomCommandMenuItemClick(TObject * /*Sender*/)
{
  AddEditCommand(false);
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::AddExtensionMenuItemClick(TObject * /*Sender*/)
{
  AddExtension();
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::EditCommandButtonClick(TObject * /*Sender*/)
{
  AddEditCommand(true);
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::AddCommandButtonDropDownClick(TObject * /*Sender*/)
{
  AddCustomCommandMenuItem->Default = (GetCommandList(CustomCommandsView->ItemIndex) == FCustomCommandList);
  AddExtensionMenuItem->Default = (GetCommandList(CustomCommandsView->ItemIndex) == FExtensionList);
  MenuPopup(AddCommandMenu, AddCommandButton);
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::CustomCommandsViewMouseMove(TObject * /*Sender*/, TShiftState /*Shift*/, int X, int Y)
{
  TListItem * Item = CustomCommandsView->GetItemAt(X, Y);
  int Index = (Item != NULL) ? Item->Index : -1;
  if (Index != FCustomCommandsHintItem)
  {
    Application->CancelHint();

    UnicodeString Hint;
    if (Index >= 0)
    {
      TCustomCommandList * List = GetCommandList(Index);
      const TCustomCommandType * Command = List->Commands[GetCommandIndex(Index)];
      Hint = StripHotkey(Command->Name);
      if (Command->ShortCut != 0)
      {
        Hint = FORMAT(L"%s (%s)", (Hint, ShortCutToText(Command->ShortCut)));
      }
      if (!Command->Description.IsEmpty())
      {
        Hint += L"\n" + Command->Description;
      }
      Hint += L"\n" + Command->GetCommandWithExpandedOptions(FCustomCommandOptions.get());
      if (List == FExtensionList)
      {
        Hint += L"\n" + Command->FileName;
      }
    }

    CustomCommandsView->Hint = Hint;

    FCustomCommandsHintItem = Index;
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::BackgroundConfirmationsLinkClick(TObject * /*Sender*/)
{
  PageControl->ActivePage = QueueSheet;
  PageControlChange(NULL);
  QueueNoConfirmationCheck->SetFocus();
  QueueNoConfirmationCheck->Perform(WM_CHANGEUISTATE, MAKEWPARAM(UIS_CLEAR, UISF_HIDEFOCUS), 0);
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::ConfigureCommandButtonClick(TObject * /*Sender*/)
{
  ConfigureCommand();
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::ConfigureCommand()
{
  int Index = CustomCommandsView->ItemIndex;
  const TCustomCommandType * Command = GetCommandList(Index)->Commands[GetCommandIndex(Index)];

  DoCustomCommandOptionsDialog(Command, FCustomCommandOptions.get(), TCustomCommandType::ofConfig, NULL);
  UpdateCustomCommandsView();
}
//---------------------------------------------------------------------------
