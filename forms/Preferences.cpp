//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Preferences.h"

#include <Common.h>
#include <ScpMain.h>

#include "VCLCommon.h"
#include "Tools.h"
#include "TextsWin.h"
#include "UserInterface.h"
#include "WinConfiguration.h"
//---------------------------------------------------------------------
#pragma link "GeneralSettings"
#pragma link "LogSettings"
#pragma link "XPGroupBox"
#pragma link "CopyParams"
#pragma link "UpDownEdit"
#pragma link "IEComboBox"
#pragma resource "*.dfm"
//---------------------------------------------------------------------
bool __fastcall DoPreferencesDialog(TPreferencesMode APreferencesMode)
{
  bool Result;
  TPreferencesDialog * PreferencesDialog = new TPreferencesDialog(Application);
  try
  {
    PreferencesDialog->PreferencesMode = APreferencesMode;
    Result = PreferencesDialog->Execute();
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
  FPreferencesMode = pmDefault;
  LoggingFrame->OnGetDefaultLogFileName = LoggingGetDefaultLogFileName;
  CopyParamsFrame->Direction = pdAll;
  FEditorFont = new TFont();
  FAfterExternalEditorDialog = false;
  FCustomCommands = new TStringList();
  FCustomCommandChanging = false;
  FCustomCommandDragDest = -1;
  UseSystemFont(this);
}
//---------------------------------------------------------------------------
__fastcall TPreferencesDialog::~TPreferencesDialog()
{
  LoggingFrame->OnGetDefaultLogFileName = NULL;
  delete FEditorFont;
  delete FCustomCommands;
}
//---------------------------------------------------------------------
bool __fastcall TPreferencesDialog::Execute()
{
  LoadConfiguration();
  bool Result = (ShowModal() == mrOk);
  if (Result) SaveConfiguration();
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::LoggingGetDefaultLogFileName(
  TObject * /*Sender*/, AnsiString & DefaultLogFileName)
{
  DefaultLogFileName = "";
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::LoadConfiguration()
{
  if (FPreferencesMode != pmLogin)
  {
    LoggingFrame->LoadConfiguration();
    GeneralSettingsFrame->LoadConfiguration();
  }
  #define BOOLPROP(PROP) PROP ## Check->Checked = WinConfiguration->PROP;
  BOOLPROP(DefaultDirIsHome);
  BOOLPROP(DeleteToRecycleBin);
  BOOLPROP(DDAllowMove);
  BOOLPROP(DDTransferConfirmation);
  BOOLPROP(DDWarnLackOfTempSpace);
  BOOLPROP(ShowHiddenFiles);
  BOOLPROP(ShowInaccesibleDirectories);
  BOOLPROP(CopyOnDoubleClick);
  BOOLPROP(CopyOnDoubleClickConfirmation);
  BOOLPROP(ConfirmOverwriting);
  BOOLPROP(ConfirmDeleting);
  BOOLPROP(ConfirmClosingSession);
  BOOLPROP(UseLocationProfiles);
  BOOLPROP(ContinueOnError);
  #undef BOOLPROP

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

  ExplorerStyleSelectionCheck->Checked =
    WinConfiguration->ScpCommander.ExplorerStyleSelection;
  PreserveLocalDirectoryCheck->Checked =
    WinConfiguration->ScpCommander.PreserveLocalDirectory;
  ShowFullAddressCheck->Checked =
    WinConfiguration->ScpExplorer.ShowFullAddress;
  RegistryStorageButton->Checked = (Configuration->Storage == stRegistry);
  IniFileStorageButton->Checked = (Configuration->Storage == stIniFile);

  RandomSeedFileEdit->Text = Configuration->RandomSeedFile;

  // editor
  EditorInternalButton->Checked = WinConfiguration->Editor.Editor == edInternal;
  EditorExternalButton->Checked = WinConfiguration->Editor.Editor == edExternal;

  AnsiString ExternalEditor = WinConfiguration->Editor.ExternalEditor;
  if (!ExternalEditor.IsEmpty())
  {
    TWinConfiguration::ReformatFileNameCommand(ExternalEditor);
  }
  ExternalEditorEdit->Text = ExternalEditor;
  EditorWordWrapCheck->Checked = WinConfiguration->Editor.WordWrap;
  FEditorFont->Name = WinConfiguration->Editor.FontName;
  FEditorFont->Height = WinConfiguration->Editor.FontHeight;
  FEditorFont->Charset = (TFontCharset)WinConfiguration->Editor.FontCharset;
  FEditorFont->Style = IntToFontStyles(WinConfiguration->Editor.FontStyle);

  CopyParamsFrame->Params = Configuration->CopyParam;
  ResumeOnButton->Checked = Configuration->CopyParam.ResumeSupport == rsOn;
  ResumeSmartButton->Checked = Configuration->CopyParam.ResumeSupport == rsSmart;
  ResumeOffButton->Checked = Configuration->CopyParam.ResumeSupport == rsOff;
  ResumeThresholdEdit->Value = Configuration->CopyParam.ResumeThreshold / 1024;

  TransferSheet->TabVisible = WinConfiguration->ExpertMode;
  GeneralSheet->TabVisible = (PreferencesMode != pmLogin) && WinConfiguration->ExpertMode;
  ExplorerSheet->TabVisible = WinConfiguration->ExpertMode;
  CommanderSheet->TabVisible = WinConfiguration->ExpertMode;
  GeneralSheet->TabVisible = (PreferencesMode != pmLogin);
  EditorSheet->TabVisible = WinConfiguration->ExpertMode && !WinConfiguration->DisableOpenEdit;

  StorageGroup->Visible = WinConfiguration->ExpertMode;
  RandomSeedFileLabel->Visible = WinConfiguration->ExpertMode;
  RandomSeedFileEdit->Visible = WinConfiguration->ExpertMode;

  FCustomCommands->Assign(WinConfiguration->CustomCommands);
  CustomCommandDescEdit->Text = "";
  CustomCommandEdit->Text = "";
  UpdateCustomCommandsView();

  PuttyPathEdit->FileName = WinConfiguration->PuttyPath;

  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::SaveConfiguration()
{
  Configuration->BeginUpdate();
  try
  {
    if (FPreferencesMode != pmLogin)
    {
      LoggingFrame->SaveConfiguration();
      GeneralSettingsFrame->SaveConfiguration();
    }
    #define BOOLPROP(PROP) WinConfiguration->PROP = PROP ## Check->Checked
    BOOLPROP(DefaultDirIsHome);
    BOOLPROP(DeleteToRecycleBin);
    BOOLPROP(DDAllowMove);
    BOOLPROP(DDTransferConfirmation);
    BOOLPROP(DDWarnLackOfTempSpace);
    BOOLPROP(ShowHiddenFiles);
    BOOLPROP(ShowInaccesibleDirectories);
    BOOLPROP(CopyOnDoubleClick);
    BOOLPROP(CopyOnDoubleClickConfirmation);
    BOOLPROP(ConfirmOverwriting);
    BOOLPROP(ConfirmDeleting);
    BOOLPROP(ConfirmClosingSession);
    BOOLPROP(UseLocationProfiles);
    BOOLPROP(ContinueOnError);
    #undef BOOLPROP

    if (DDSystemTemporaryDirectoryButton->Checked)
    {
      WinConfiguration->DDTemporaryDirectory = "";
    }
    else
    {
      WinConfiguration->DDTemporaryDirectory = DDTemporaryDirectoryEdit->Text;
    }

    Configuration->Storage = RegistryStorageButton->Checked ? stRegistry : stIniFile;

    TScpCommanderConfiguration ScpCommander = WinConfiguration->ScpCommander;
    ScpCommander.ExplorerStyleSelection = ExplorerStyleSelectionCheck->Checked;
    ScpCommander.PreserveLocalDirectory = PreserveLocalDirectoryCheck->Checked;
    WinConfiguration->ScpCommander = ScpCommander;

    TScpExplorerConfiguration ScpExplorer = WinConfiguration->ScpExplorer;
    ScpExplorer.ShowFullAddress = ShowFullAddressCheck->Checked;
    WinConfiguration->ScpExplorer = ScpExplorer;

    Configuration->RandomSeedFile = RandomSeedFileEdit->Text;

    // editor
    WinConfiguration->Editor.Editor =
      (EditorInternalButton->Checked || ExternalEditorEdit->Text.IsEmpty()) ?
        edInternal : edExternal;
    WinConfiguration->Editor.ExternalEditor = ExternalEditorEdit->Text;
    WinConfiguration->Editor.WordWrap = EditorWordWrapCheck->Checked;
    WinConfiguration->Editor.FontName = FEditorFont->Name;
    WinConfiguration->Editor.FontHeight = FEditorFont->Height;
    WinConfiguration->Editor.FontCharset = FEditorFont->Charset;
    WinConfiguration->Editor.FontStyle = FontStylesToInt(FEditorFont->Style);

    TCopyParamType CopyParam = CopyParamsFrame->Params;
    if (ResumeOnButton->Checked) CopyParam.ResumeSupport = rsOn;
    if (ResumeSmartButton->Checked) CopyParam.ResumeSupport = rsSmart;
    if (ResumeOffButton->Checked) CopyParam.ResumeSupport = rsOff;
    CopyParam.ResumeThreshold = ResumeThresholdEdit->Value * 1024;
    Configuration->CopyParam = CopyParam;

    WinConfiguration->CustomCommands = FCustomCommands;

    WinConfiguration->PuttyPath = PuttyPathEdit->FileName;
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
    
    GeneralSheet->TabVisible = (value != pmLogin);
    LogSheet->TabVisible = (value != pmLogin);
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::FormShow(TObject * /*Sender*/)
{
  switch (PreferencesMode) {
    case pmEditor: PageControl->ActivePage = EditorSheet; break;
    case pmCustomCommands: PageControl->ActivePage = CustomCommandsSheet; break;
    default: PageControl->ActivePage = PreferencesSheet; break;
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::ControlChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::UpdateControls()
{
  EnableControl(CopyOnDoubleClickConfirmationCheck, CopyOnDoubleClickCheck->Checked);
  EnableControl(DDTemporaryDirectoryEdit, DDCustomTemporaryDirectoryButton->Checked);
  EnableControl(ResumeThresholdEdit, ResumeSmartButton->Checked);

  EditorFontLabel->Caption = FORMAT("%s, %d pt",
    (FEditorFont->Name, FEditorFont->Size));

  bool CommandComplete = !CustomCommandDescEdit->Text.IsEmpty() &&
    !CustomCommandEdit->Text.IsEmpty();
  EnableControl(AddCommandButton, CommandComplete);
  EnableControl(SaveCommandButton, CommandComplete &&
    CustomCommandsView->Selected &&
    (CustomCommandDescEdit->Text != FCustomCommands->Names[CustomCommandsView->ItemIndex] ||
     CustomCommandEdit->Text != FCustomCommands->Values[
      FCustomCommands->Names[CustomCommandsView->ItemIndex]]));
  EnableControl(RemoveCommandButton, CustomCommandsView->Selected);
  EnableControl(UpCommandButton, CustomCommandsView->ItemIndex > 0);
  EnableControl(DownCommandButton, CustomCommandsView->ItemIndex >= 0 &&
    CustomCommandsView->ItemIndex < CustomCommandsView->Items->Count - 1);
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::EditorFontButtonClick(TObject * /*Sender*/)
{
  TFontDialog * Dialog = new TFontDialog(Application);
  try
  {
    Dialog->Device = fdScreen;
    Dialog->Options = TFontDialogOptions() << fdForceFontExist;
    Dialog->Font = FEditorFont;
    if (Dialog->Execute())
    {
      FEditorFont->Assign(Dialog->Font);
      UpdateControls();
    }
  }
  __finally
  {
    delete Dialog;
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::ExternalEditorEditExit(TObject * /*Sender*/)
{
  try
  {
    AnsiString ExternalEditor = ExternalEditorEdit->Text;
    if (!ExternalEditor.IsEmpty())
    {
      TWinConfiguration::ReformatFileNameCommand(ExternalEditor);
      ExternalEditorEdit->Text = ExternalEditor;
    }
  }
  catch(...)
  {
    ExternalEditorEdit->SelectAll();
    ExternalEditorEdit->SetFocus();
    throw;
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::ExternalEditorEditAfterDialog(
      TObject * /*Sender*/, AnsiString & /*Name*/, bool & Action)
{
  if (Action)
  {
    FAfterExternalEditorDialog = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::ExternalEditorEditChange(
      TObject *Sender)
{
  if (FAfterExternalEditorDialog)
  {
    FAfterExternalEditorDialog = false;
    ExternalEditorEditExit(Sender);
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::FormCloseQuery(TObject * /*Sender*/,
      bool & /*CanClose*/)
{
  if (ModalResult != mrCancel && ExternalEditorEdit->Focused())
  {
    ExternalEditorEditExit(NULL);
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::IconButtonClick(TObject *Sender)
{
  if (MessageDialog(LoadStr(CONFIRM_CREATE_ICON),
        qtConfirmation, qaYes | qaNo, 0) == qaYes)
  {
    AnsiString IconName, Params;
    int SpecialFolder;
    if (Sender == SendToHookButton)
    {
      IconName = FMTLOAD(SENDTO_HOOK_NAME, (AppNameVersion));
      SpecialFolder = CSIDL_SENDTO;
      Params = "/upload";
    }
    else if (Sender == QuickLaunchIconButton)
    {
      IconName = "Microsoft\\Internet Explorer\\Quick Launch\\" +
        AppNameVersion;
      SpecialFolder = CSIDL_APPDATA;
    }
    else
    {
      IconName = AppNameVersion;
      SpecialFolder = Sender == DesktopIconButton ?
        CSIDL_DESKTOPDIRECTORY :CSIDL_COMMON_DESKTOPDIRECTORY;
    }
    CreateDesktopShortCut(IconName,
      Application->ExeName, Params, "", SpecialFolder);
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::CustomCommandsViewData(TObject * /*Sender*/,
      TListItem * Item)
{
  assert(FCustomCommands);
  int Index = Item->Index;
  assert(Index >= 0 && Index <= FCustomCommands->Count);
  Item->Caption = StringReplace(FCustomCommands->Names[Index], "&", "",
    TReplaceFlags() << rfReplaceAll);
  assert(!Item->SubItems->Count);
  Item->SubItems->Add(FCustomCommands->Values[FCustomCommands->Names[Index]]);
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::CustomCommandsViewSelectItem(
      TObject * /*Sender*/, TListItem * Item, bool Selected)
{
  if (Item && Selected)
  {
    assert(Item);
    int Index = Item->Index;
    assert(Index >= 0 && Index <= FCustomCommands->Count);

    CustomCommandDescEdit->Text = FCustomCommands->Names[Index];
    CustomCommandEdit->Text = FCustomCommands->Values[FCustomCommands->Names[Index]];
  }
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::UpdateCustomCommandsView()
{
  CustomCommandsView->Items->Count = FCustomCommands->Count;
  AdjustListColumnsWidth(CustomCommandsView);
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
}
//---------------------------------------------------------------------------
AnsiString __fastcall TPreferencesDialog::CustomCommandString(int Index)
{
  if (CustomCommandDescEdit->Text.Pos("="))
  {
    throw Exception(FMTLOAD(CUSTOM_COMMAND_INVALID, ("=")));
  }
  int I = FCustomCommands->IndexOfName(CustomCommandDescEdit->Text);
  if (I >= 0 && (Index < 0 || I != Index))
  {
    throw Exception(FMTLOAD(CUSTOM_COMMAND_DUPLICATE, (CustomCommandDescEdit->Text)));
  }
  return FORMAT("%s=%s", (CustomCommandDescEdit->Text, CustomCommandEdit->Text));
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::AddCommandButtonClick(TObject * /*Sender*/)
{
  int Index;
  if (CustomCommandsView->ItemIndex >= 0)
  {
    FCustomCommands->Insert(CustomCommandsView->ItemIndex, CustomCommandString());
    Index = CustomCommandsView->ItemIndex;
  }
  else
  {
    Index = FCustomCommands->Add(CustomCommandString());
  }
  CustomCommandsView->ItemIndex = Index;
  UpdateCustomCommandsView();
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::SaveCommandButtonClick(TObject * /*Sender*/)
{
  assert(CustomCommandsView->ItemIndex >= 0 &&
    CustomCommandsView->ItemIndex < FCustomCommands->Count);
  FCustomCommands->Strings[CustomCommandsView->ItemIndex] =
    CustomCommandString(CustomCommandsView->ItemIndex);
  UpdateCustomCommandsView();
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::RemoveCommandButtonClick(
      TObject * /*Sender*/)
{
  assert(CustomCommandsView->ItemIndex >= 0 &&
    CustomCommandsView->ItemIndex < FCustomCommands->Count);
  FCustomCommands->Delete(CustomCommandsView->ItemIndex);
  UpdateCustomCommandsView();
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::CustomCommandMove(int Source, int Dest)
{
  if (Source >= 0 && Source < FCustomCommands->Count &&
      Dest >= 0 && Dest < FCustomCommands->Count)
  {
    FCustomCommands->Move(Source, Dest);
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
void __fastcall TPreferencesDialog::CustomCommandsViewStartDrag(
      TObject * /*Sender*/, TDragObject *& /*DragObject*/)
{
  FCustomCommandDragSource = CustomCommandsView->ItemIndex;
  FCustomCommandDragDest = -1;
}
//---------------------------------------------------------------------------
bool __fastcall TPreferencesDialog::AllowCustomCommandsDrag(int X, int Y)
{
  TListItem * Item = CustomCommandsView->GetItemAt(X, Y);
  FCustomCommandDragDest = Item ? Item->Index : -1;
  return (FCustomCommandDragDest >= 0) && (FCustomCommandDragDest != FCustomCommandDragSource);
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::CustomCommandsViewDragDrop(
      TObject * /*Sender*/, TObject * Source, int X, int Y)
{
  if (Source == CustomCommandsView)
  {
    if (AllowCustomCommandsDrag(X, Y))
    {
      CustomCommandMove(FCustomCommandDragSource, FCustomCommandDragDest);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::CustomCommandsViewDragOver(
      TObject * /*Sender*/, TObject * Source, int /*X*/, int /*Y*/,
      TDragState /*State*/, bool & Accept)
{
  if (Source == CustomCommandsView)
  {
    // cannot use AllowCustomCommandsDrag(X, Y) because of bug in VCL
    // (when dropped on item itself, when it was dragged over another item before,
    // that another item remains highlighted forever)
    Accept = true;
  }
}
//---------------------------------------------------------------------------

