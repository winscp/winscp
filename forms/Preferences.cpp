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
//---------------------------------------------------------------------
#pragma link "GeneralSettings"
#pragma link "LogSettings"
#pragma link "XPGroupBox"
#pragma link "CopyParams"
#pragma link "UpDownEdit"
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
  UseSystemFont(this);
}
//---------------------------------------------------------------------------
__fastcall TPreferencesDialog::~TPreferencesDialog()
{
  LoggingFrame->OnGetDefaultLogFileName = NULL;
  delete FEditorFont;
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
  #define BOOLPROP(PROP) PROP ## Check->Checked = Configuration->PROP;
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
  #undef BOOLPROP

  if (Configuration->DDTemporaryDirectory.IsEmpty())
  {
    DDSystemTemporaryDirectoryButton->Checked = true;
    DDTemporaryDirectoryEdit->Text = SystemTemporaryDirectory();
  }
  else
  {
    DDCustomTemporaryDirectoryButton->Checked = true;
    DDTemporaryDirectoryEdit->Text = Configuration->DDTemporaryDirectory;
  }

  ExplorerStyleSelectionCheck->Checked =
    Configuration->ScpCommander.ExplorerStyleSelection;
  ShowFullAddressCheck->Checked =
    Configuration->ScpExplorer.ShowFullAddress;
  RegistryStorageButton->Checked = (Configuration->Storage == stRegistry);
  IniFileStorageButton->Checked = (Configuration->Storage == stIniFile);

  RandomSeedFileEdit->Text = Configuration->RandomSeedFile;

  // editor
  EditorInternalButton->Checked = Configuration->Editor.Editor == edInternal;
  EditorExternalButton->Checked = Configuration->Editor.Editor == edExternal;

  AnsiString ExternalEditor = Configuration->Editor.ExternalEditor;
  if (!ExternalEditor.IsEmpty())
  {
    TConfiguration::ReformatFileNameCommand(ExternalEditor);
  }
  ExternalEditorEdit->Text = ExternalEditor;
  EditorWordWrapCheck->Checked = Configuration->Editor.WordWrap;
  FEditorFont->Name = Configuration->Editor.FontName;
  FEditorFont->Height = Configuration->Editor.FontHeight;
  FEditorFont->Charset = (TFontCharset)Configuration->Editor.FontCharset;
  FEditorFont->Style = IntToFontStyles(Configuration->Editor.FontStyle);

  CopyParamsFrame->Params = Configuration->CopyParam;
  ResumeOnButton->Checked = Configuration->CopyParam.ResumeSupport == rsOn;
  ResumeSmartButton->Checked = Configuration->CopyParam.ResumeSupport == rsSmart;
  ResumeOffButton->Checked = Configuration->CopyParam.ResumeSupport == rsOff;
  ResumeThresholdEdit->Value = Configuration->CopyParam.ResumeThreshold / 1024;

  TransferSheet->TabVisible = Configuration->ExpertMode;
  GeneralSheet->TabVisible = (PreferencesMode != pmLogin) && Configuration->ExpertMode;
  ExplorerSheet->TabVisible = Configuration->ExpertMode;
  CommanderSheet->TabVisible = Configuration->ExpertMode;
  GeneralSheet->TabVisible = (PreferencesMode != pmLogin);
  EditorSheet->TabVisible = Configuration->ExpertMode;

  StorageGroup->Visible = Configuration->ExpertMode;
  RandomSeedFileLabel->Visible = Configuration->ExpertMode;
  RandomSeedFileEdit->Visible = Configuration->ExpertMode;

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
    #define BOOLPROP(PROP) Configuration->PROP = PROP ## Check->Checked
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
    #undef BOOLPROP

    if (DDSystemTemporaryDirectoryButton->Checked)
    {
      Configuration->DDTemporaryDirectory = "";
    }
    else
    {
      Configuration->DDTemporaryDirectory = DDTemporaryDirectoryEdit->Text;
    }

    Configuration->Storage = RegistryStorageButton->Checked ? stRegistry : stIniFile;

    TScpCommanderConfiguration ScpCommander = Configuration->ScpCommander;
    ScpCommander.ExplorerStyleSelection = ExplorerStyleSelectionCheck->Checked;
    Configuration->ScpCommander = ScpCommander;

    TScpExplorerConfiguration ScpExplorer = Configuration->ScpExplorer;
    ScpExplorer.ShowFullAddress = ShowFullAddressCheck->Checked;
    Configuration->ScpExplorer = ScpExplorer;

    Configuration->RandomSeedFile = RandomSeedFileEdit->Text;

    // editor
    Configuration->Editor.Editor =
      (EditorInternalButton->Checked || ExternalEditorEdit->Text.IsEmpty()) ?
        edInternal : edExternal;
    Configuration->Editor.ExternalEditor = ExternalEditorEdit->Text;
    Configuration->Editor.WordWrap = EditorWordWrapCheck->Checked;
    Configuration->Editor.FontName = FEditorFont->Name;
    Configuration->Editor.FontHeight = FEditorFont->Height;
    Configuration->Editor.FontCharset = FEditorFont->Charset;
    Configuration->Editor.FontStyle = FontStylesToInt(FEditorFont->Style);

    TCopyParamType CopyParam = CopyParamsFrame->Params;
    if (ResumeOnButton->Checked) CopyParam.ResumeSupport = rsOn;
    if (ResumeSmartButton->Checked) CopyParam.ResumeSupport = rsSmart;
    if (ResumeOffButton->Checked) CopyParam.ResumeSupport = rsOff;
    CopyParam.ResumeThreshold = ResumeThresholdEdit->Value * 1024;
    Configuration->CopyParam = CopyParam;
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
  PageControl->ActivePage =
    (PreferencesMode == pmEditor) ? EditorSheet : PreferencesSheet;
  //DefaultDirIsHomeCheck->SetFocus();
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
      TConfiguration::ReformatFileNameCommand(ExternalEditor);
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

