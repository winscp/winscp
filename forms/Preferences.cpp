//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>

#include "Preferences.h"

#include <ScpMain.h>
#include <Terminal.h>

#include "VCLCommon.h"
#include "GUITools.h"
#include "Tools.h"
#include "TextsWin.h"
#include "WinInterface.h"
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
  FCustomCommands = new TCustomCommands();
  FCustomCommandChanging = false;
  FCustomCommandDragDest = -1;
  UseSystemSettings(this);
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
            Tree->Items->Item[i]->Text = PageControl->Pages[pi]->Hint;
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
  BOOLPROP(DDTransferConfirmation);
  BOOLPROP(DDWarnLackOfTempSpace);
  BOOLPROP(ShowHiddenFiles);
  BOOLPROP(ShowInaccesibleDirectories);
  BOOLPROP(CopyOnDoubleClick);
  BOOLPROP(CopyOnDoubleClickConfirmation);
  BOOLPROP(ConfirmOverwriting);
  BOOLPROP(ConfirmDeleting);
  BOOLPROP(ConfirmClosingSession);
  BOOLPROP(ConfirmExitOnCompletion);
  BOOLPROP(UseLocationProfiles);
  BOOLPROP(ContinueOnError);
  BOOLPROP(DDAllowMoveInit);
  #undef BOOLPROP

  CompareByTimeCheck->Checked = WinConfiguration->ScpCommander.CompareByTime;
  CompareBySizeCheck->Checked = WinConfiguration->ScpCommander.CompareBySize;

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
  ExternalEditorTextCheck->Checked = WinConfiguration->Editor.ExternalEditorText;
  EditorWordWrapCheck->Checked = WinConfiguration->Editor.WordWrap;
  FEditorFont->Name = WinConfiguration->Editor.FontName;
  FEditorFont->Height = WinConfiguration->Editor.FontHeight;
  FEditorFont->Charset = (TFontCharset)WinConfiguration->Editor.FontCharset;
  FEditorFont->Style = IntToFontStyles(WinConfiguration->Editor.FontStyle);

  CopyParamsFrame->Params = GUIConfiguration->CopyParam;
  ResumeOnButton->Checked = GUIConfiguration->CopyParam.ResumeSupport == rsOn;
  ResumeSmartButton->Checked = GUIConfiguration->CopyParam.ResumeSupport == rsSmart;
  ResumeOffButton->Checked = GUIConfiguration->CopyParam.ResumeSupport == rsOff;
  ResumeThresholdEdit->Value = GUIConfiguration->CopyParam.ResumeThreshold / 1024;

  TransferSheet->Enabled = WinConfiguration->ExpertMode;
  GeneralSheet->Enabled = (PreferencesMode != pmLogin) && WinConfiguration->ExpertMode;
  ExplorerSheet->Enabled = WinConfiguration->ExpertMode;
  CommanderSheet->Enabled = WinConfiguration->ExpertMode;
  GeneralSheet->Enabled = (PreferencesMode != pmLogin);
  EditorSheet->Enabled = WinConfiguration->ExpertMode && !WinConfiguration->DisableOpenEdit;

  StorageGroup->Visible = WinConfiguration->ExpertMode;
  RandomSeedFileLabel->Visible = WinConfiguration->ExpertMode;
  RandomSeedFileEdit->Visible = WinConfiguration->ExpertMode;

  FCustomCommands->Assign(WinConfiguration->CustomCommands);
  CustomCommandDescEdit->Text = "";
  CustomCommandEdit->Text = "";
  UpdateCustomCommandsView();

  PuttyPathEdit->FileName = WinConfiguration->PuttyPath;

  // Queue
  QueueTransferLimitEdit->AsInteger = GUIConfiguration->QueueTransfersLimit;
  QueueAutoPopupCheck->Checked = GUIConfiguration->QueueAutoPopup;
  QueueCheck->Checked = GUIConfiguration->CopyParam.Queue;
  RememberPasswordCheck->Checked = Configuration->RememberPassword;
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

  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::SaveConfiguration()
{
  Configuration->BeginUpdate();
  try
  {
    TGUICopyParamType CopyParam = GUIConfiguration->CopyParam;

    if (FPreferencesMode != pmLogin)
    {
      LoggingFrame->SaveConfiguration();
      GeneralSettingsFrame->SaveConfiguration();
    }
    #define BOOLPROP(PROP) WinConfiguration->PROP = PROP ## Check->Checked
    BOOLPROP(DefaultDirIsHome);
    BOOLPROP(DeleteToRecycleBin);
    BOOLPROP(DDTransferConfirmation);
    BOOLPROP(DDWarnLackOfTempSpace);
    BOOLPROP(ShowHiddenFiles);
    BOOLPROP(ShowInaccesibleDirectories);
    BOOLPROP(CopyOnDoubleClick);
    BOOLPROP(CopyOnDoubleClickConfirmation);
    BOOLPROP(ConfirmOverwriting);
    BOOLPROP(ConfirmDeleting);
    BOOLPROP(ConfirmClosingSession);
    BOOLPROP(ConfirmExitOnCompletion);
    BOOLPROP(UseLocationProfiles);
    BOOLPROP(ContinueOnError);
    BOOLPROP(DDAllowMoveInit);
    #undef BOOLPROP

    WinConfiguration->ScpCommander.CompareByTime = CompareByTimeCheck->Checked;
    WinConfiguration->ScpCommander.CompareBySize = CompareBySizeCheck->Checked;
    WinConfiguration->DDAllowMove = !DDWarnOnMoveCheck->Checked;
    WinConfiguration->DDExtEnabled = DDExtEnabledButton->Checked;

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
    WinConfiguration->Editor.ExternalEditorText = ExternalEditorTextCheck->Checked;
    WinConfiguration->Editor.WordWrap = EditorWordWrapCheck->Checked;
    WinConfiguration->Editor.FontName = FEditorFont->Name;
    WinConfiguration->Editor.FontHeight = FEditorFont->Height;
    WinConfiguration->Editor.FontCharset = FEditorFont->Charset;
    WinConfiguration->Editor.FontStyle = FontStylesToInt(FEditorFont->Style);

    // overwrites only TCopyParamType fields
    CopyParam = CopyParamsFrame->Params;
    if (ResumeOnButton->Checked) CopyParam.ResumeSupport = rsOn;
    if (ResumeSmartButton->Checked) CopyParam.ResumeSupport = rsSmart;
    if (ResumeOffButton->Checked) CopyParam.ResumeSupport = rsOff;
    CopyParam.ResumeThreshold = ResumeThresholdEdit->Value * 1024;

    WinConfiguration->CustomCommands = FCustomCommands;

    WinConfiguration->PuttyPath = PuttyPathEdit->FileName;

    // Queue
    GUIConfiguration->QueueTransfersLimit = QueueTransferLimitEdit->AsInteger;
    GUIConfiguration->QueueAutoPopup = QueueAutoPopupCheck->Checked;
    CopyParam.Queue = QueueCheck->Checked;
    Configuration->RememberPassword = RememberPasswordCheck->Checked;

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
    
    GUIConfiguration->CopyParam = CopyParam;
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
  PrepareNavigationTree(NavigationTree);

  for (int Index = 0; Index < PageControl->PageCount; Index++)
  {
    PageControl->Pages[Index]->TabVisible = false;
  }
  // change form height by height of hidden tabs
  ClientHeight -= 50;

  switch (PreferencesMode) {
    case pmEditor: PageControl->ActivePage = EditorSheet; break;
    case pmCustomCommands: PageControl->ActivePage = CustomCommandsSheet; break;
    case pmQueue: PageControl->ActivePage = QueueSheet; break;
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
void __fastcall TPreferencesDialog::UpdateControls()
{
  EnableControl(CopyOnDoubleClickConfirmationCheck, CopyOnDoubleClickCheck->Checked);
  EnableControl(ResumeThresholdEdit, ResumeSmartButton->Checked);

  EditorFontLabel->Caption = FMTLOAD(EDITOR_FONT_FMT,
    (FEditorFont->Name, FEditorFont->Size));

  bool CommandComplete = !CustomCommandDescEdit->Text.IsEmpty() &&
    !CustomCommandEdit->Text.IsEmpty();
  EnableControl(AddCommandButton, CommandComplete);
  EnableControl(SaveCommandButton, CommandComplete &&
    CustomCommandsView->Selected &&
    (CustomCommandDescEdit->Text != FCustomCommands->Names[CustomCommandsView->ItemIndex] ||
     CustomCommandEdit->Text != FCustomCommands->Values[
      FCustomCommands->Names[CustomCommandsView->ItemIndex]] ||
     CustomCommandParams() != FCustomCommands->Params[
      FCustomCommands->Names[CustomCommandsView->ItemIndex]]));
  EnableControl(RemoveCommandButton, CustomCommandsView->Selected);
  EnableControl(UpCommandButton, CustomCommandsView->ItemIndex > 0);
  EnableControl(DownCommandButton, CustomCommandsView->ItemIndex >= 0 &&
    CustomCommandsView->ItemIndex < CustomCommandsView->Items->Count - 1);

  EnableControl(DDExtEnabledButton, WinConfiguration->DDExtInstalled);
  EnableControl(DDExtEnabledLabel, WinConfiguration->DDExtInstalled);
  EnableControl(DDExtDisabledPanel, DDExtDisabledButton->Checked);
  EnableControl(DDTemporaryDirectoryEdit, DDCustomTemporaryDirectoryButton->Enabled && 
    DDCustomTemporaryDirectoryButton->Checked);
  EnableControl(DDWarnOnMoveCheck, DDExtDisabledButton->Checked &&
    DDAllowMoveInitCheck->Checked);
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
  AnsiString Name = FCustomCommands->Names[Index];
  Item->SubItems->Add(FCustomCommands->Values[Name]);
  int Params = FCustomCommands->Params[Name];
  AnsiString ParamsStr;
  if ((Params & ccApplyToDirectories) && (Params & ccRecursive))
  {
    ParamsStr = FMTLOAD(CUSTOM_COMMAND_PARAMFMT,
      (LoadStr(CUSTOM_COMMAND_DIRECTORIES), LoadStr(CUSTOM_COMMAND_RECURSE)));
  }
  else if (Params & ccApplyToDirectories)
  {
    ParamsStr = LoadStr(CUSTOM_COMMAND_DIRECTORIES);
  }
  else if (Params & ccRecursive)
  {
    ParamsStr = LoadStr(CUSTOM_COMMAND_RECURSE);
  }
  Item->SubItems->Add(ParamsStr);
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
    AnsiString Name = FCustomCommands->Names[Index];
    CustomCommandEdit->Text = FCustomCommands->Values[Name];
    int Params = FCustomCommands->Params[Name];
    CustomCommandApplyToDirectoriesCheck->Checked = Params & ccApplyToDirectories;
    CustomCommandRecursiveCheck->Checked = Params & ccRecursive;
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
int __fastcall TPreferencesDialog::CustomCommandParams()
{
  return
    (CustomCommandApplyToDirectoriesCheck->Checked ? ccApplyToDirectories : 0) |
    (CustomCommandRecursiveCheck->Checked ? ccRecursive : 0);
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
  FCustomCommands->Params[CustomCommandDescEdit->Text] = CustomCommandParams();
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
  FCustomCommands->Params[CustomCommandDescEdit->Text] = CustomCommandParams();
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
void __fastcall TPreferencesDialog::CompareByTimeCheckClick(
      TObject * /*Sender*/)
{
  if (!CompareByTimeCheck->Checked)
  {
    CompareBySizeCheck->Checked = true;
  }
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::CompareBySizeCheckClick(
      TObject * /*Sender*/)
{
  if (!CompareBySizeCheck->Checked)
  {
    CompareByTimeCheck->Checked = true;
  }
  UpdateControls();
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
void __fastcall TPreferencesDialog::Dispatch(void *Message)
{
  TMessage * M = reinterpret_cast<TMessage*>(Message);
  assert(M);
  if (M->Msg == CM_DIALOGKEY)
  {
    CMDialogKey(*((TWMKeyDown *)Message));
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
        qtConfirmation, qaYes | qaNo, 0) == qaYes)
  {
    RegisterAsUrlHandler();
  }
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::DDExtLabelClick(TObject * Sender)
{
  ((Sender == DDExtEnabledLabel) ? DDExtEnabledButton : DDExtDisabledButton)->
    Checked = true;
}
//---------------------------------------------------------------------------
void __fastcall TPreferencesDialog::PathEditsKeyDown(
  TObject * Sender, WORD & Key, TShiftState Shift)
{
  PathEditKeyDown(dynamic_cast<TCustomEdit*>(Sender), Key, Shift, false);
}
//---------------------------------------------------------------------------

