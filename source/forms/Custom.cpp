//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Dialogs.hpp>
//---------------------------------------------------------------------
#include <Common.h>
#include <CustomWinConfiguration.h>
#include <WinInterface.h>
#include <VCLCommon.h>
#include <TextsWin.h>
#include <HelpWin.h>
#include <CoreMain.h>
#include <PasTools.hpp>
#include <ProgParams.h>
#include <Tools.h>
#include <GUITools.h>
#include <PuttyTools.h>
#include <HistoryComboBox.hpp>
#include <Math.hpp>
#include <System.Character.hpp>

#include "Custom.h"
//---------------------------------------------------------------------
#pragma link "PasswordEdit"
#pragma resource "*.dfm"
//---------------------------------------------------------------------
const int GroupBoxBorderWidth = 1;
//---------------------------------------------------------------------
__fastcall TCustomDialog::TCustomDialog(UnicodeString AHelpKeyword)
  : TForm(GetFormOwner())
{
  UseSystemSettings(this);

  FControlPadding = ScaleByTextHeight(this, 6);
  FPos = ScaleByTextHeight(this, 8);
  FPrePos = FPos;
  FHorizontalMargin = ScaleByTextHeight(this, 8);
  FIndent = FHorizontalMargin;
  FGroupBox = NULL;

  HelpKeyword = AHelpKeyword;

  TBorderIcons BI = BorderIcons;
  if (HelpKeyword.IsEmpty())
  {
    BI >> biHelp;

    OKButton->Left = CancelButton->Left;
    CancelButton->Left = HelpButton->Left;
    HelpButton->Visible = false;
  }
  else
  {
    BI << biHelp;
  }
  BorderIcons = BI;
}
//---------------------------------------------------------------------
bool __fastcall TCustomDialog::Execute()
{
  Changed();
  return (ShowModal() == DefaultResult(this));
}
//---------------------------------------------------------------------
void __fastcall TCustomDialog::DoChange(bool & /*CanSubmit*/)
{
  // noop
}
//---------------------------------------------------------------------
void __fastcall TCustomDialog::Changed()
{
  bool CanSubmit = true;
  DoChange(CanSubmit);
  EnableControl(OKButton, CanSubmit);
}
//---------------------------------------------------------------------
void __fastcall TCustomDialog::Change(TObject * /*Sender*/)
{
  Changed();
}
//---------------------------------------------------------------------------
void __fastcall TCustomDialog::DoHelp()
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
void __fastcall TCustomDialog::HelpButtonClick(TObject * /*Sender*/)
{
  DoHelp();
}
//---------------------------------------------------------------------------
void __fastcall TCustomDialog::DoShow()
{
  OKButton->TabOrder = FCount;
  CancelButton->TabOrder = static_cast<short>(FCount + 1);
  HelpButton->TabOrder = static_cast<short>(FCount + 2);
  Changed();
  TForm::DoShow();
}
//---------------------------------------------------------------------------
void __fastcall TCustomDialog::DoValidate()
{
  // noop
}
//---------------------------------------------------------------------------
bool __fastcall TCustomDialog::CloseQuery()
{
  if (ModalResult == DefaultResult(this))
  {
    DoValidate();
  }
  return TForm::CloseQuery();
}
//---------------------------------------------------------------------------
void __fastcall TCustomDialog::RemoveCancelButton()
{
  CancelButton->Visible = false;
  OKButton->Left = CancelButton->Left;
  OKButton->Cancel = true;
  DebugAssert(OKButton->Width == CancelButton->Width);
  DebugAssert(OKButton->Top == CancelButton->Top);
}
//---------------------------------------------------------------------------
void __fastcall TCustomDialog::AddDialogButton(TButton * Button)
{
  Button->Parent = this;
  Button->Top = OKButton->Top;
  Button->Left = FHorizontalMargin;
  Button->Height = OKButton->Height;
  AddWinControl(Button);
}
//---------------------------------------------------------------------------
void __fastcall TCustomDialog::AddImage(const UnicodeString & ImageName)
{
  TImage * Image = new TImage(this);
  Image->Name = L"Image";
  Image->Parent = GetDefaultParent();
  LoadDialogImage(Image, ImageName);
  Image->SetBounds(FIndent, FPos + ScaleByTextHeight(this, 3), Image->Picture->Width, Image->Picture->Height);
  FIndent += Image->Width + ScaleByTextHeight(this, 12);
}
//---------------------------------------------------------------------------
int __fastcall TCustomDialog::GetMaxControlWidth(TControl * Control)
{
  return GetDefaultParent()->ClientWidth - Control->Left - FHorizontalMargin - (FGroupBox != NULL ? GroupBoxBorderWidth : 0);
}
//---------------------------------------------------------------------------
TWinControl * __fastcall TCustomDialog::GetDefaultParent()
{
  return (FGroupBox != NULL) ? FGroupBox : static_cast<TWinControl *>(this);
}
//---------------------------------------------------------------------------
void __fastcall TCustomDialog::AdjustHeight(TControl * Control)
{
  FPos = Control->Top + Control->Height + FControlPadding;
  int Delta = (FPos - FPrePos);
  ClientHeight = ClientHeight + Delta;
  if (FGroupBox != NULL)
  {
    FGroupBox->Height = FGroupBox->Height + Delta;
  }
  FPrePos = FPos;
}
//---------------------------------------------------------------------------
void __fastcall TCustomDialog::AddWinControl(TWinControl * Control)
{
  Control->TabOrder = FCount;
  FCount++;
}
//---------------------------------------------------------------------------
TCheckBox * __fastcall TCustomDialog::CreateAndAddCheckBox(const UnicodeString & Caption)
{
  TCheckBox * CheckBox = new TCheckBox(this);
  CheckBox->Caption = Caption;
  AddButtonControl(CheckBox);
  return CheckBox;
}
//---------------------------------------------------------------------------
TLabel * __fastcall TCustomDialog::CreateLabel(UnicodeString Label)
{
  TLabel * Result = new TUIStateAwareLabel(this);
  Result->Caption = Label;
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomDialog::AddEditLikeControl(TWinControl * Edit, TLabel * Label, bool OneLine)
{
  Edit->Parent = GetDefaultParent();
  // this updates Height property to real value
  Edit->HandleNeeded();

  if (Label != NULL)
  {
    Label->Parent = GetDefaultParent();
    Label->Left = FIndent;

    if (OneLine)
    {
      DebugAssert(Edit->Height > Label->Height);
      Label->Top = FPos + ((Edit->Height - Label->Height) / 2);
    }
    else
    {
      Label->Top = FPos;

      FPos += Label->Height + ScaleByTextHeight(this, 3);
    }
  }

  Edit->Top = FPos;
  if (OneLine)
  {
    Edit->Left = GetDefaultParent()->ClientWidth - FHorizontalMargin - Edit->Width;
  }
  else
  {
    Edit->Left = FIndent;
    Edit->Width = GetMaxControlWidth(Edit);
  }

  AdjustHeight(Edit);

  if (Label != NULL)
  {
    if (Label->FocusControl == NULL)
    {
      Label->FocusControl = Edit;
    }
    else
    {
      DebugAssert(Label->FocusControl == Edit);
    }
  }

  AddWinControl(Edit);
}
//---------------------------------------------------------------------------
void __fastcall TCustomDialog::AddEdit(TCustomEdit * Edit, TLabel * Label, bool OneLine)
{
  AddEditLikeControl(Edit, Label, OneLine);

  TEdit * PublicEdit = reinterpret_cast<TEdit *>(Edit);
  if (PublicEdit->OnChange == NULL)
  {
    PublicEdit->OnChange = Change;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomDialog::SetUpComboBox(TCustomCombo * Combo, TStrings * Items, bool OneLine)
{
  if (Items != NULL)
  {
    Combo->Items = Items;
  }

  if (OneLine)
  {
    int Width = 0;
    for (int Index = 0; Index < Combo->Items->Count; Index++)
    {
      Width = Max(Width, Combo->Canvas->TextWidth(Combo->Items->Strings[Index]));
    }

    Width += ScaleByTextHeight(Combo, 4 + 16 + 14);
    Width = Max(Width, HelpButton->Width);

    Combo->Width = Width;
    Combo->Left = GetDefaultParent()->ClientWidth - FHorizontalMargin - Width;
  }

  TComboBox * PublicCombo = reinterpret_cast<TComboBox *>(Combo);
  if (PublicCombo->OnChange == NULL)
  {
    PublicCombo->OnChange = Change;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomDialog::AddComboBox(TCustomCombo * Combo, TLabel * Label, TStrings * Items, bool OneLine)
{
  AddEditLikeControl(Combo, Label, OneLine);

  SetUpComboBox(Combo, Items, OneLine);
}
//---------------------------------------------------------------------------
void __fastcall TCustomDialog::AddShortCutComboBox(TComboBox * Combo, TLabel * Label, const TShortCuts & ShortCuts)
{
  AddEditLikeControl(Combo, Label, true);
  InitializeShortCutCombo(Combo, ShortCuts);
  SetUpComboBox(Combo, NULL, true);
}
//---------------------------------------------------------------------------
void __fastcall TCustomDialog::ScaleButtonControl(TButtonControl * Control)
{
  // this updates Height property to real value
  Control->HandleNeeded();
  // buttons do not scale with text on their own
  Control->Height = ScaleByTextHeight(Control, Control->Height);
}
//---------------------------------------------------------------------------
void __fastcall TCustomDialog::AddButtonControl(TButtonControl * Control)
{
  Control->Parent = GetDefaultParent();
  Control->Left = FIndent + ScaleByTextHeight(this, 2);
  Control->Top = FPos;
  Control->Width = GetMaxControlWidth(Control);
  ScaleButtonControl(Control);

  AdjustHeight(Control);

  AddWinControl(Control);

  TCheckBox * PublicControl = reinterpret_cast<TCheckBox *>(Control);
  if (PublicControl->OnClick == NULL)
  {
    PublicControl->OnClick = Change;
  }
}
//---------------------------------------------------------------------------
void TCustomDialog::AddButtonNextToEdit(TButton * Button, TWinControl * Edit)
{
  Button->Parent = GetDefaultParent();
  Button->Width = HelpButton->Width;
  Button->Left = GetDefaultParent()->ClientWidth - Button->Width - HorizontalMargin;
  Edit->Width = Button->Left - Edit->Left - ScaleByTextHeight(this, 6);
  Button->Top = Edit->Top - ScaleByTextHeight(this, 1);
  ScaleButtonControl(Button);
  AddWinControl(Button);
}
//---------------------------------------------------------------------------
void __fastcall TCustomDialog::AddText(TLabel * Label)
{
  Label->Parent = GetDefaultParent();

  DebugAssert(Label->AutoSize);
  Label->WordWrap = true;
  Label->Left = FIndent;
  Label->Width = GetMaxControlWidth(Label);
  Label->Top = FPos;
  Label->ShowAccelChar = false;

  AutoSizeLabel(Label);

  AdjustHeight(Label);
}
//---------------------------------------------------------------------------
void __fastcall TCustomDialog::AddText(TStaticText * Label)
{
  Label->Parent = GetDefaultParent();

  DebugAssert(Label->AutoSize);
  Label->Left = FIndent;
  Label->Top = FPos;
  Label->ShowAccelChar = false;

  AdjustHeight(Label);
  AddWinControl(Label);
}
//---------------------------------------------------------------------------
void __fastcall TCustomDialog::AddSeparator()
{
  TBevel * Bevel = new TBevel(this);
  Bevel->Parent = GetDefaultParent();

  Bevel->Left = FIndent;
  Bevel->Top = FPos;
  Bevel->Height = 2;
  Bevel->Width = GetMaxControlWidth(Bevel);

  AdjustHeight(Bevel);
}
//---------------------------------------------------------------------------
void __fastcall TCustomDialog::StartGroup(const UnicodeString & Caption)
{
  if (FGroupBox != NULL)
  {
    FIndent = FGroupBox->Left;
    FPos = FGroupBox->Top + FGroupBox->Height + FControlPadding;
    FPrePos = FPos;
    FGroupBox = NULL;
  }

  TGroupBox * GroupBox = new TGroupBox(this);
  GroupBox->Parent = this;
  GroupBox->Caption = Caption;

  GroupBox->Left = FIndent;
  GroupBox->Top = FPos;
  GroupBox->Height = ScaleByTextHeight(GroupBox, 26);
  GroupBox->Width = GetMaxControlWidth(GroupBox);

  AdjustHeight(GroupBox);

  AddWinControl(GroupBox);

  // but if the first control is oneline box, then we should roll back a bit
  FPos = ScaleByTextHeight(this, 22);
  FPrePos = FPos;
  FIndent = FHorizontalMargin + GroupBoxBorderWidth;

  FGroupBox = GroupBox;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
class TSaveSessionDialog : public TCustomDialog
{
public:
  __fastcall TSaveSessionDialog(TComponent* AOwner);
  void __fastcall Init(bool CanSavePassword, bool NotRecommendedSavingPassword,
    TStrings * AdditionalFolders);

  bool __fastcall Execute(UnicodeString & SessionName, bool & SavePassword,
    bool & CreateShortcut, const UnicodeString & OriginalSessionName);

protected:
  virtual void __fastcall DoValidate();
  virtual void __fastcall DoChange(bool & CanSubmit);

private:
  UnicodeString FOriginalSessionName;
  TEdit * SessionNameEdit;
  TComboBox * FolderCombo;
  TCheckBox * SavePasswordCheck;
  TCheckBox * CreateShortcutCheck;
  UnicodeString FRootFolder;

  UnicodeString __fastcall GetSessionName();
};
//---------------------------------------------------------------------------
// Need to have an Owner argument for SafeFormCreate
__fastcall TSaveSessionDialog::TSaveSessionDialog(TComponent* /*AOwner*/) :
  TCustomDialog(HELP_SESSION_SAVE)
{
}
//---------------------------------------------------------------------------
void __fastcall TSaveSessionDialog::Init(bool CanSavePassword,
  bool NotRecommendedSavingPassword, TStrings * AdditionalFolders)
{
  Caption = LoadStr(SAVE_SESSION_CAPTION);

  SessionNameEdit = new TEdit(this);
  AddEdit(SessionNameEdit, CreateLabel(LoadStr(SAVE_SESSION_PROMPT)));

  FRootFolder = LoadStr(SAVE_SESSION_ROOT_FOLDER2);
  std::unique_ptr<TStringList> Folders(new TStringList());

  if (AdditionalFolders != NULL)
  {
    Folders->AddStrings(AdditionalFolders);
  }

  for (int Index = 0; Index < StoredSessions->Count; Index++)
  {
    TSessionData * Data = StoredSessions->Sessions[Index];
    if (!Data->Special && !Data->IsWorkspace)
    {
      UnicodeString Folder = Data->FolderName;
      if (!Folder.IsEmpty() && Folders->IndexOf(Folder) < 0)
      {
        Folders->Add(Folder);
      }
    }
  }

  DebugAssert(!Folders->CaseSensitive);
  Folders->Sort();

  FolderCombo = new TUIStateAwareComboBox(this);
  AddComboBox(FolderCombo, CreateLabel(LoadStr(SAVE_SESSION_FOLDER)));
  FolderCombo->DropDownCount = Max(FolderCombo->DropDownCount, 16);
  FolderCombo->Items->Add(FRootFolder);
  FolderCombo->Items->AddStrings(Folders.get());

  SavePasswordCheck = CreateAndAddCheckBox(
    LoadStr(NotRecommendedSavingPassword ? SAVE_SESSION_PASSWORD :
      (CustomWinConfiguration->UseMasterPassword ? SAVE_SESSION_PASSWORD_MASTER : SAVE_SESSION_PASSWORD_RECOMMENDED)));

  CreateShortcutCheck = CreateAndAddCheckBox(LoadStr(SAVE_SITE_WORKSPACE_SHORTCUT));

  EnableControl(SavePasswordCheck, CanSavePassword);
}
//---------------------------------------------------------------------------
bool __fastcall TSaveSessionDialog::Execute(
  UnicodeString & SessionName, bool & SavePassword, bool & CreateShortcut,
  const UnicodeString & OriginalSessionName)
{
  FOriginalSessionName = OriginalSessionName;
  SessionNameEdit->Text = TSessionData::ExtractLocalName(SessionName);
  UnicodeString Folder = TSessionData::ExtractFolderName(SessionName);
  if (Folder.IsEmpty())
  {
    FolderCombo->Text = FRootFolder;
  }
  else
  {
    FolderCombo->Text = Folder;
  }
  SavePasswordCheck->Checked = SavePassword;
  CreateShortcutCheck->Checked = CreateShortcut;
  bool Result = TCustomDialog::Execute();
  if (Result)
  {
    SessionName = GetSessionName();
    SavePassword = SavePasswordCheck->Checked;
    CreateShortcut = CreateShortcutCheck->Checked;
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TSaveSessionDialog::GetSessionName()
{
  UnicodeString Folder;
  if (FolderCombo->Text != FRootFolder)
  {
    Folder = FolderCombo->Text;
  }
  return TSessionData::ComposePath(Folder, SessionNameEdit->Text);
}
//---------------------------------------------------------------------------
void __fastcall TSaveSessionDialog::DoValidate()
{
  TSessionData::ValidateName(SessionNameEdit->Text);
  SessionNameValidate(GetSessionName(), FOriginalSessionName);

  UnicodeString Folder = TSessionData::ExtractFolderName(GetSessionName());
  if (!Folder.IsEmpty() && StoredSessions->IsWorkspace(Folder))
  {
    throw Exception(FMTLOAD(WORKSPACE_NOT_FOLDER, (Folder)));
  }

  if (SavePasswordCheck->Enabled && SavePasswordCheck->Checked &&
      CustomWinConfiguration->UseMasterPassword)
  {
    CustomWinConfiguration->AskForMasterPasswordIfNotSet();
  }

  TCustomDialog::DoValidate();
}
//---------------------------------------------------------------------------
void __fastcall TSaveSessionDialog::DoChange(bool & CanSubmit)
{
  CanSubmit = !SessionNameEdit->Text.IsEmpty();
  TCustomDialog::DoChange(CanSubmit);
}
//---------------------------------------------------------------------------
TSessionData * __fastcall DoSaveSession(TSessionData * SessionData,
  TSessionData * OriginalSession, bool ForceDialog,
  TStrings * AdditionalFolders)
{
  bool SavePassword = false;
  bool * PSavePassword;
  bool NotRecommendedSavingPassword =
    !CustomWinConfiguration->UseMasterPassword &&
    !SameText(SessionData->UserName, AnonymousUserName);

  if (Configuration->DisablePasswordStoring ||
      !SessionData->HasAnySessionPassword())
  {
    PSavePassword = NULL;
  }
  else
  {
    PSavePassword = &SavePassword;
    SavePassword =
      ((OriginalSession != NULL) && OriginalSession->HasAnySessionPassword()) ||
      !NotRecommendedSavingPassword;
  }

  UnicodeString SessionName = SessionData->SessionName;

  bool Result;
  bool CreateShortcut = false;
  if (!ForceDialog && ((PSavePassword == NULL) || SavePassword))
  {
    // This is probably here to ask before session is started saving.
    // Otherwise we would ask implicitly, when saving passwords, but at that moment,
    // part of the site is already saved and when the user cancel the prompt it's too late.
    CustomWinConfiguration->AskForMasterPasswordIfNotSetAndNeededToPersistSessionData(SessionData);
    Result = true;
  }
  else
  {
    // This can be a standalone dialog when used with save URL (from GetLoginData)
    TSaveSessionDialog * Dialog = SafeFormCreate<TSaveSessionDialog>();
    try
    {
      Dialog->Init((PSavePassword != NULL), NotRecommendedSavingPassword, AdditionalFolders);
      Result = Dialog->Execute(SessionName, SavePassword, CreateShortcut, SessionData->Name);
    }
    __finally
    {
      delete Dialog;
    }
  }

  TSessionData * NewSession = NULL;
  if (Result)
  {
    if ((PSavePassword != NULL) && !SavePassword)
    {
      SessionData->ClearSessionPasswords();
    }

    NewSession =
      StoredSessions->NewSession(SessionName, SessionData);
    // modified only, explicit
    StoredSessions->Save(false, true);
    if (!SessionData->HostKey.IsEmpty())
    {
      SessionData->CacheHostKeyIfNotCached();
    }

    if (CreateShortcut)
    {
      TOperationVisualizer Visualizer;
      UnicodeString AdditionalParams =
        TProgramParams::FormatSwitch(DESKTOP_SWITCH) + L" " +
        TProgramParams::FormatSwitch(UPLOAD_IF_ANY_SWITCH);
      CreateDesktopSessionShortCut(SessionName, L"", AdditionalParams, -1, SITE_ICON);
    }
  }

  return NewSession;
}
//---------------------------------------------------------------------------
void __fastcall SessionNameValidate(const UnicodeString & Text,
  const UnicodeString & OriginalName)
{
  TSessionData::ValidatePath(Text);

  DebugAssert(StoredSessions);
  TSessionData * Data = (TSessionData *)StoredSessions->FindByName(Text);
  if (Data && Data->Special)
  {
    MessageDialog(FMTLOAD(CANNOT_OVERWRITE_SPECIAL_SESSION, (Text)),
      qtError, qaOK, HELP_NONE);
    Abort();
  }
  else if ((Data != NULL) && !Data->IsSameName(OriginalName) &&
    MessageDialog(MainInstructions(FMTLOAD(CONFIRM_OVERWRITE_SESSION, (Text))),
      qtConfirmation, qaYes | qaNo, HELP_SESSION_SAVE_OVERWRITE) != qaYes)
  {
    Abort();
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
class TSaveWorkspaceDialog : public TCustomDialog
{
public:
  __fastcall TSaveWorkspaceDialog(bool CanSavePasswords,
    bool NotRecommendedSavingPasswords);

  bool __fastcall Execute(
    UnicodeString & WorkspaceName, bool & SavePasswords, bool & CreateShortcut,
    bool & EnableAutoSave);

protected:
  virtual void __fastcall DoValidate();
  virtual void __fastcall DoChange(bool & CanSubmit);

private:
  TComboBox * WorkspaceNameCombo;
  TCheckBox * SavePasswordsCheck;
  TCheckBox * CreateShortcutCheck;
  TCheckBox * EnableAutoSaveCheck;
};
//---------------------------------------------------------------------------
__fastcall TSaveWorkspaceDialog::TSaveWorkspaceDialog(
    bool CanSavePasswords, bool NotRecommendedSavingPasswords) :
  TCustomDialog(HELP_WORKSPACE_SAVE)
{
  Caption = LoadStr(SAVE_WORKSPACE_CAPTION);

  WorkspaceNameCombo = new TUIStateAwareComboBox(this);
  WorkspaceNameCombo->AutoComplete = false;
  AddComboBox(WorkspaceNameCombo, CreateLabel(LoadStr(SAVE_WORKSPACE_PROMPT)));
  WorkspaceNameCombo->DropDownCount = Max(WorkspaceNameCombo->DropDownCount, 16);

  std::unique_ptr<TStrings> Workspaces(StoredSessions->GetWorkspaces());
  WorkspaceNameCombo->Items->AddStrings(Workspaces.get());

  SavePasswordsCheck = CreateAndAddCheckBox(
    LoadStr(NotRecommendedSavingPasswords ? SAVE_WORKSPACE_PASSWORDS :
      (CustomWinConfiguration->UseMasterPassword ?
        SAVE_WORKSPACE_PASSWORDS_MASTER : SAVE_WORKSPACE_PASSWORDS_RECOMMENDED)));

  EnableControl(SavePasswordsCheck, CanSavePasswords);

  CreateShortcutCheck = CreateAndAddCheckBox(LoadStr(SAVE_SITE_WORKSPACE_SHORTCUT));

  EnableAutoSaveCheck = CreateAndAddCheckBox(LoadStr(SAVE_WORKSPACE_AUTO));
}
//---------------------------------------------------------------------------
bool __fastcall TSaveWorkspaceDialog::Execute(
  UnicodeString & WorkspaceName, bool & SavePasswords, bool & CreateShortcut,
  bool & EnableAutoSave)
{
  WorkspaceNameCombo->Text = WorkspaceName;
  SavePasswordsCheck->Checked = SavePasswords;
  CreateShortcutCheck->Checked = CreateShortcut;
  EnableAutoSaveCheck->Checked = EnableAutoSave;
  bool Result = TCustomDialog::Execute();
  if (Result)
  {
    WorkspaceName = WorkspaceNameCombo->Text;
    SavePasswords = SavePasswordsCheck->Checked;
    CreateShortcut = CreateShortcutCheck->Checked;
    EnableAutoSave = EnableAutoSaveCheck->Checked;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TSaveWorkspaceDialog::DoValidate()
{
  TSessionData::ValidateName(WorkspaceNameCombo->Text);

  if (StoredSessions->IsFolder(WorkspaceNameCombo->Text))
  {
    throw Exception(FMTLOAD(FOLDER_NOT_WORKSPACE, (WorkspaceNameCombo->Text)));
  }

  if (SavePasswordsCheck->Enabled && SavePasswordsCheck->Checked &&
      CustomWinConfiguration->UseMasterPassword)
  {
    CustomWinConfiguration->AskForMasterPasswordIfNotSet();
  }

  TCustomDialog::DoValidate();
}
//---------------------------------------------------------------------------
void __fastcall TSaveWorkspaceDialog::DoChange(bool & CanSubmit)
{
  CanSubmit = !WorkspaceNameCombo->Text.IsEmpty();

  TCustomDialog::DoChange(CanSubmit);
}
//---------------------------------------------------------------------------
bool __fastcall DoSaveWorkspaceDialog(UnicodeString & WorkspaceName,
  bool * SavePasswords, bool NotRecommendedSavingPasswords,
  bool & CreateShortcut, bool & EnableAutoSave)
{
  std::unique_ptr<TSaveWorkspaceDialog> Dialog(
    new TSaveWorkspaceDialog((SavePasswords != NULL), NotRecommendedSavingPasswords));

  bool Dummy = false;
  if (SavePasswords == NULL)
  {
    SavePasswords = &Dummy;
  }
  return
    Dialog->Execute(
      WorkspaceName, *SavePasswords, CreateShortcut, EnableAutoSave);
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
class TShortCutDialog : public TCustomDialog
{
public:
  __fastcall TShortCutDialog(const TShortCuts & ShortCuts, UnicodeString HelpKeyword);

  bool __fastcall Execute(TShortCut & ShortCut);

private:
  TComboBox * ShortCutCombo;
};
//---------------------------------------------------------------------------
__fastcall TShortCutDialog::TShortCutDialog(const TShortCuts & ShortCuts, UnicodeString HelpKeyword) :
  TCustomDialog(HelpKeyword)
{
  Caption = LoadStr(SHORTCUT_CAPTION);

  ShortCutCombo = new TUIStateAwareComboBox(this);
  AddShortCutComboBox(ShortCutCombo, CreateLabel(LoadStr(SHORTCUT_LABEL)), ShortCuts);
}
//---------------------------------------------------------------------------
bool __fastcall TShortCutDialog::Execute(TShortCut & ShortCut)
{
  SetShortCutCombo(ShortCutCombo, ShortCut);
  bool Result = TCustomDialog::Execute();
  if (Result)
  {
    ShortCut = GetShortCutCombo(ShortCutCombo);
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall DoShortCutDialog(TShortCut & ShortCut,
  const TShortCuts & ShortCuts, UnicodeString HelpKeyword)
{
  bool Result;
  TShortCutDialog * Dialog = new TShortCutDialog(ShortCuts, HelpKeyword);
  try
  {
    Result = Dialog->Execute(ShortCut);
  }
  __finally
  {
    delete Dialog;
  }
  return Result;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
class TRemoteMoveDialog : public TCustomDialog
{
public:
  __fastcall TRemoteMoveDialog(bool Multi, TDirectoryExistsEvent OnDirectoryExists);

  bool __fastcall Execute(UnicodeString & Target, UnicodeString & FileMask);

protected:
  DYNAMIC void __fastcall DoShow();
  virtual void __fastcall DoValidate();
  UnicodeString __fastcall GetFileMask();

private:
  THistoryComboBox * Combo;
  bool FMulti;
  TDirectoryExistsEvent FOnDirectoryExists;
};
//---------------------------------------------------------------------------
__fastcall TRemoteMoveDialog::TRemoteMoveDialog(bool Multi, TDirectoryExistsEvent OnDirectoryExists) :
  TCustomDialog(HELP_REMOTE_MOVE)
{
  Caption = LoadStr(REMOTE_MOVE_TITLE);
  // The same as TRemoteTransferDialog
  ClientWidth = ScaleByTextHeight(this, 466);

  FMulti = Multi;
  FOnDirectoryExists = OnDirectoryExists;

  AddImage(L"Move L to R");

  Combo = new THistoryComboBox(this);
  Combo->AutoComplete = false;
  AddComboBox(Combo, CreateLabel(LoadStr(REMOTE_TRANSFER_PROMPT2)));
}
//---------------------------------------------------------------------------
bool __fastcall TRemoteMoveDialog::Execute(UnicodeString & Target, UnicodeString & FileMask)
{
  Combo->Items = CustomWinConfiguration->History[L"RemoteTarget"];
  Combo->Text = UnixIncludeTrailingBackslash(Target) + FileMask;
  bool Result = TCustomDialog::Execute();
  if (Result)
  {
    Target = UnixExtractFilePath(Combo->Text);
    FileMask = GetFileMask();
    Combo->SaveToHistory();
    CustomWinConfiguration->History[L"RemoteTarget"] = Combo->Items;
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TRemoteMoveDialog::GetFileMask()
{
  return UnixExtractFileName(Combo->Text);
}
//---------------------------------------------------------------------------
void __fastcall TRemoteMoveDialog::DoShow()
{
  TCustomDialog::DoShow();
  InstallPathWordBreakProc(Combo);
}
//---------------------------------------------------------------------------
void __fastcall TRemoteMoveDialog::DoValidate()
{
  if (FOnDirectoryExists(NULL, Combo->Text))
  {
    Combo->Text = UnixCombinePaths(Combo->Text, AnyMask);
  }

  if (!IsFileNameMask(GetFileMask()) && FMulti)
  {
    UnicodeString Message =
      FormatMultiFilesToOneConfirmation(Combo->Text, true);
    if (MessageDialog(Message, qtConfirmation, qaOK | qaCancel, HELP_NONE) == qaCancel)
    {
      Abort();
    }
  }

  TCustomDialog::DoValidate();
}
//---------------------------------------------------------------------------
bool __fastcall DoRemoteMoveDialog(
  bool Multi, UnicodeString & Target, UnicodeString & FileMask, TDirectoryExistsEvent OnDirectoryExists)
{
  std::unique_ptr<TRemoteMoveDialog> Dialog(new TRemoteMoveDialog(Multi, OnDirectoryExists));
  return Dialog->Execute(Target, FileMask);
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
class TCustomCommandOptionsDialog : public TCustomDialog
{
public:
  __fastcall TCustomCommandOptionsDialog(
    const TCustomCommandType * Command, TStrings * CustomCommandOptions, unsigned int Flags,
    TCustomCommand * CustomCommandForOptions, const UnicodeString & Site, const TShortCuts * ShortCuts);

  bool __fastcall Execute(TShortCut * ShortCut);

protected:
  virtual void __fastcall DoHelp();
  DYNAMIC void __fastcall DoShow();

private:
  const TCustomCommandType * FCommand;
  TStrings * FCustomCommandOptions;
  std::vector<TControl *> FControls;
  std::vector<std::vector<UnicodeString> > FValues;
  unsigned int FFlags;
  UnicodeString FSite;
  TComboBox * FShortCutCombo;

  UnicodeString __fastcall HistoryKey(const TCustomCommandType::TOption & Option);
  THistoryComboBox * __fastcall CreateHistoryComboBox(const TCustomCommandType::TOption & Option, const UnicodeString & Value);
  void __fastcall BrowseButtonClick(TObject * Sender);
  void __fastcall LinkLabelClick(TObject * Sender);
  UnicodeString __fastcall SaveHistoryComboBoxValue(TControl * Control, const TCustomCommandType::TOption & Option);
  void __fastcall AddOptionComboBox(
    TComboBox * ComboBox, const UnicodeString & Value, const TCustomCommandType::TOption & Option,
    std::vector<UnicodeString> & Values);
  UnicodeString __fastcall GetComboBoxValue(TControl * Control, const UnicodeString & Default);
  int __fastcall GetOptionIndex(TControl * Control);
  int __fastcall GetControlIndex(TControl * Control);
};
//---------------------------------------------------------------------------
__fastcall TCustomCommandOptionsDialog::TCustomCommandOptionsDialog(
    const TCustomCommandType * Command, TStrings * CustomCommandOptions,
    unsigned int Flags, TCustomCommand * CustomCommandForOptions,
    const UnicodeString & Site, const TShortCuts * ShortCuts) :
  TCustomDialog(HELP_EXTENSION_OPTIONS)
{
  FCommand = Command;
  FFlags = Flags;
  FCustomCommandOptions = CustomCommandOptions;
  FSite = Site;
  Caption = StripEllipsis(StripHotkey(FCommand->Name));
  Width = ScaleByTextHeight(this, 444);

  bool HasGroups = false;
  int ControlIndex = 0;
  for (int OptionIndex = 0; OptionIndex < FCommand->OptionsCount; OptionIndex++)
  {
    const TCustomCommandType::TOption & Option = FCommand->GetOption(OptionIndex);

    if ((Option.Flags & FFlags) != 0)
    {
      UnicodeString OptionKey = FCommand->GetOptionKey(Option, FSite);
      UnicodeString Value;
      if ((CustomCommandForOptions != NULL) &&
          Option.HasPatterns(CustomCommandForOptions))
      {
        Value = CustomCommandForOptions->Complete(Option.Default, true);
      }
      else
      {
        if (FCustomCommandOptions->IndexOfName(OptionKey) >= 0)
        {
          Value = FCustomCommandOptions->Values[OptionKey];
        }
        else
        {
          Value = Option.Default;
        }
      }

      int Tag = (OptionIndex << 16) + ControlIndex;
      TControl * Control = NULL;
      std::vector<UnicodeString> Values;
      if (Option.Kind == TCustomCommandType::okUnknown)
      {
        Control = NULL;
      }
      else if (Option.Kind == TCustomCommandType::okLabel)
      {
        TLabel * Label = CreateLabel(Option.Caption);
        AddText(Label);
        Control = Label;
      }
      else if (Option.Kind == TCustomCommandType::okLink)
      {
        TStaticText * Label = new TStaticText(this);
        Label->Caption = Option.Caption;
        if (IsHttpOrHttpsUrl(Label->Caption))
        {
          Label->Caption = SecureUrl(Label->Caption);
          LinkLabel(Label);
          Label->TabStop = true;
        }
        else if (!Option.Default.IsEmpty() && IsHttpOrHttpsUrl(Option.Default))
        {
          Label->OnClick = LinkLabelClick;
          LinkLabel(Label);
          Label->TabStop = true;
        }
        else
        {
          // keep it plain text, as we have no URL
        }
        AddText(Label);
        Control = Label;
      }
      else if (Option.Kind == TCustomCommandType::okGroup)
      {
        StartGroup(Option.Caption);
        HasGroups = true;
      }
      else if (Option.Kind == TCustomCommandType::okSeparator)
      {
        AddSeparator();
      }
      else if (Option.Kind == TCustomCommandType::okTextBox)
      {
        Control = CreateHistoryComboBox(Option, Value);
      }
      else if (Option.Kind == TCustomCommandType::okFile)
      {
        THistoryComboBox * ComboBox = CreateHistoryComboBox(Option, Value);
        TButton * Button = new TButton(this);
        AddButtonNextToEdit(Button, ComboBox);
        Button->Tag = Tag;
        Button->Caption = LoadStr(EXTENSION_OPTIONS_BROWSE);
        Button->OnClick = BrowseButtonClick;
        Control = ComboBox;
      }
      else if (Option.Kind == TCustomCommandType::okDropDownList)
      {
        TComboBox * ComboBox = new TUIStateAwareComboBox(this);
        ComboBox->Style = csDropDownList;

        AddOptionComboBox(ComboBox, Value, Option, Values);

        Control = ComboBox;
      }
      else if (Option.Kind == TCustomCommandType::okComboBox)
      {
        TComboBox * ComboBox = new TUIStateAwareComboBox(this);
        ComboBox->Style = csDropDown;

        AddOptionComboBox(ComboBox, Value, Option, Values);
        if (ComboBox->ItemIndex < 0)
        {
          ComboBox->Text = Value;
        }

        Control = ComboBox;
      }
      else if (Option.Kind == TCustomCommandType::okCheckBox)
      {
        TCheckBox * CheckBox = CreateAndAddCheckBox(Option.Caption);

        CheckBox->Checked =
          (Option.Params.size() >= 1) &&
          (Value == Option.Params[0]);

        Control = CheckBox;
      }
      else
      {
        DebugFail();
      }

      if (Control != NULL)
      {
        Control->Tag = Tag;
      }
      FControls.push_back(Control);
      FValues.push_back(Values);
      ControlIndex++;
      DebugAssert(static_cast<int>(FControls.size()) == ControlIndex);
    }
  }

  if (ShortCuts != NULL)
  {
    if (HasGroups)
    {
      StartGroup(LoadStr(EXTENSION_GENERAL_GROUP));
    }
    else if (ControlIndex > 0)
    {
      AddSeparator();
    }
    FShortCutCombo = new TUIStateAwareComboBox(this);
    AddShortCutComboBox(FShortCutCombo, CreateLabel(LoadStr(EXTENSION_SHORTCUT)), *ShortCuts);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandOptionsDialog::AddOptionComboBox(
  TComboBox * ComboBox, const UnicodeString & Value, const TCustomCommandType::TOption & Option, std::vector<UnicodeString> & Values)
{
  std::unique_ptr<TStringList> Items(new TStringList());
  int ItemIndex = -1;

  TCustomCommandType::TOption::TParams::const_iterator ParamI = Option.Params.begin();
  while (ParamI != Option.Params.end())
  {
    UnicodeString Item = (*ParamI);
    int P = Item.Pos(L"=");
    UnicodeString ParamValue;
    if (P > 0)
    {
      ParamValue = Item.SubString(1, P - 1);
      Item.Delete(1, P);
    }
    else
    {
      ParamValue = Item;
    }
    Item = WinConfiguration->ExtensionStringTranslation(FCommand->Id, Item);
    Items->Add(Item);
    if (Value == ParamValue)
    {
      ItemIndex = Items->Count - 1;
    }
    Values.push_back(ParamValue);
    ParamI++;
  }

  AddComboBox(ComboBox, CreateLabel(Option.Caption), Items.get(), true);

  ComboBox->ItemIndex = ItemIndex;
}
//---------------------------------------------------------------------------
int __fastcall TCustomCommandOptionsDialog::GetOptionIndex(TControl * Control)
{
  return (Control->Tag >> 16);
}
//---------------------------------------------------------------------------
int __fastcall TCustomCommandOptionsDialog::GetControlIndex(TControl * Control)
{
  return (Control->Tag & 0xFFFF);
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandOptionsDialog::LinkLabelClick(TObject * Sender)
{
  TStaticText * Label = DebugNotNull(dynamic_cast<TStaticText *>(Sender));
  const TCustomCommandType::TOption & Option = FCommand->GetOption(GetOptionIndex(Label));
  OpenBrowser(SecureUrl(Option.Default));
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandOptionsDialog::BrowseButtonClick(TObject * Sender)
{
  TButton * Button = DebugNotNull(dynamic_cast<TButton *>(Sender));
  int OptionIndex = GetOptionIndex(Button);
  const TCustomCommandType::TOption & Option = FCommand->GetOption(OptionIndex);
  int ControlIndex = GetControlIndex(Button);
  THistoryComboBox * ComboBox = dynamic_cast<THistoryComboBox *>(FControls[ControlIndex]);

  std::unique_ptr<TOpenDialog> OpenDialog(new TOpenDialog(Application));

  UnicodeString Title;
  if (!Option.FileCaption.IsEmpty())
  {
    Title = Option.FileCaption;
  }
  else
  {
    UnicodeString Caption = Option.Caption;
    Caption = StripHotkey(Caption);
    if (!Caption.IsEmpty() && (Caption[Caption.Length()] == L':'))
    {
      Caption.SetLength(Caption.Length() - 1);
    }
    Title = FMTLOAD(EXTENSION_OPTIONS_BROWSE_TITLE, (Caption));
  }
  OpenDialog->Title = Title;

  UnicodeString Value;
  if (ComboBox->Text.IsEmpty())
  {
    Value = Option.FileInitial;
  }
  else
  {
    Value = ComboBox->Text;
  }
  UnicodeString ExpandedValue = ExpandEnvironmentVariables(Value);
  OpenDialog->FileName = ExpandedValue;
  UnicodeString InitialDir = ExtractFilePath(ExpandedValue);
  if (!InitialDir.IsEmpty())
  {
    OpenDialog->InitialDir = InitialDir;
  }
  OpenDialog->Filter = Option.FileFilter;
  OpenDialog->DefaultExt = Option.FileExt;

  if (OpenDialog->Execute())
  {
    if (OpenDialog->FileName != ExpandedValue)
    {
      ComboBox->Text = OpenDialog->FileName;
    }
    // If user just confirms the initial value, persist it
    else if (ComboBox->Text.IsEmpty())
    {
      DebugAssert(Option.FileInitial == Value);
      ComboBox->Text = Value;
    }
  }
}
//---------------------------------------------------------------------------
THistoryComboBox * __fastcall TCustomCommandOptionsDialog::CreateHistoryComboBox(
  const TCustomCommandType::TOption & Option, const UnicodeString & Value)
{
  THistoryComboBox * ComboBox = new THistoryComboBox(this);
  ComboBox->AutoComplete = false;
  AddComboBox(ComboBox, CreateLabel(Option.Caption));
  ComboBox->Items = CustomWinConfiguration->History[HistoryKey(Option)];
  ComboBox->Text = Value;
  return ComboBox;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCustomCommandOptionsDialog::HistoryKey(const TCustomCommandType::TOption & Option)
{
  UnicodeString Result = FCommand->GetOptionKey(Option, FSite);
  Result = CustomWinConfiguration->GetValidHistoryKey(Result);
  return L"CustomCommandOption_" + Result;
}
//---------------------------------------------------------------------------
bool __fastcall TCustomCommandOptionsDialog::Execute(TShortCut * ShortCut)
{
  if (ShortCut != NULL)
  {
    SetShortCutCombo(FShortCutCombo, *ShortCut);
  }

  bool Result = TCustomDialog::Execute();

  if (Result)
  {
    int ControlIndex = 0;
    for (int OptionIndex = 0; OptionIndex < FCommand->OptionsCount; OptionIndex++)
    {
      const TCustomCommandType::TOption & Option = FCommand->GetOption(OptionIndex);
      if ((Option.Flags & FFlags) != 0)
      {
        if ((Option.Kind != TCustomCommandType::okUnknown) &&
            Option.IsControl)
        {
          UnicodeString OptionKey = FCommand->GetOptionKey(Option, FSite);

          TControl * Control = FControls[ControlIndex];

          UnicodeString Value;
          if (Option.Kind == TCustomCommandType::okTextBox)
          {
            Value = SaveHistoryComboBoxValue(Control, Option);
          }
          else if (Option.Kind == TCustomCommandType::okFile)
          {
            Value = SaveHistoryComboBoxValue(Control, Option);
          }
          else if (Option.Kind == TCustomCommandType::okDropDownList)
          {
            Value = GetComboBoxValue(Control, Option.Default);
          }
          else if (Option.Kind == TCustomCommandType::okComboBox)
          {
            TComboBox * ComboBox = DebugNotNull(dynamic_cast<TComboBox *>(Control));
            Value = GetComboBoxValue(Control, ComboBox->Text);
          }
          else if (Option.Kind == TCustomCommandType::okCheckBox)
          {
            TCheckBox * CheckBox = DebugNotNull(dynamic_cast<TCheckBox *>(Control));
            int Index = (CheckBox->Checked ? 0 : 1);
            Value = (Index < static_cast<int>(Option.Params.size())) ? Option.Params[Index] : UnicodeString();
          }
          else
          {
            DebugFail();
          }

          // The default value setter deletes the "name" when the value is empty.
          // It would cause us to fall back to the default value, but we want to remember the empty value.
          SetStringValueEvenIfEmpty(FCustomCommandOptions, OptionKey, Value);
        }

        ControlIndex++;
      }
    }

    if (ShortCut != NULL)
    {
      *ShortCut = GetShortCutCombo(FShortCutCombo);
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCustomCommandOptionsDialog::GetComboBoxValue(
  TControl * Control, const UnicodeString & Default)
{
  TComboBox * ComboBox = DebugNotNull(dynamic_cast<TComboBox *>(Control));
  UnicodeString Result;
  if (ComboBox->ItemIndex < 0)
  {
    Result = Default;
  }
  else
  {
    Result = FValues[GetControlIndex(Control)][ComboBox->ItemIndex];
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCustomCommandOptionsDialog::SaveHistoryComboBoxValue(
  TControl * Control, const TCustomCommandType::TOption & Option)
{
  THistoryComboBox * ComboBox = DebugNotNull(dynamic_cast<THistoryComboBox *>(Control));
  ComboBox->SaveToHistory();
  CustomWinConfiguration->History[HistoryKey(Option)] = ComboBox->Items;
  return ComboBox->Text;
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandOptionsDialog::DoHelp()
{
  UnicodeString HelpPage;
  if (!FCommand->OptionsPage.IsEmpty())
  {
    HelpPage = FCommand->OptionsPage;
  }
  else
  {
    HelpPage = FCommand->HomePage;
  }

  if (!HelpPage.IsEmpty())
  {
    OpenBrowser(HelpPage);
  }
  else
  {
    TCustomDialog::DoHelp();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandOptionsDialog::DoShow()
{
  TCustomDialog::DoShow();

  int ControlIndex = 0;
  for (int OptionIndex = 0; OptionIndex < FCommand->OptionsCount; OptionIndex++)
  {
    const TCustomCommandType::TOption & Option = FCommand->GetOption(OptionIndex);

    if ((Option.Flags & FFlags) != 0)
    {
      if (Option.Kind == TCustomCommandType::okFile)
      {
        TControl * Control = FControls[ControlIndex];
        InstallPathWordBreakProc(DebugNotNull(dynamic_cast<TWinControl *>(Control)));
      }
      ControlIndex++;
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall DoCustomCommandOptionsDialog(
  const TCustomCommandType * Command, TStrings * CustomCommandOptions, TShortCut * ShortCut,
  unsigned int Flags, TCustomCommand * CustomCommandForOptions,
  const UnicodeString & Site, const TShortCuts * ShortCuts)
{
  std::unique_ptr<TCustomCommandOptionsDialog> Dialog(
    new TCustomCommandOptionsDialog(Command, CustomCommandOptions, Flags, CustomCommandForOptions, Site, ShortCuts));
  return Dialog->Execute(ShortCut);
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
class TUsageStatisticsDialog : public TCustomDialog
{
public:
  __fastcall TUsageStatisticsDialog();

protected:
  virtual void __fastcall DoChange(bool & CanSubmit);

private:
  TEdit * FilterEdit;
  TMemo * UsageMemo;
  TButton * ClipboardButton;

  void __fastcall ClipboardButtonClick(TObject * Sender);
};
//---------------------------------------------------------------------------
__fastcall TUsageStatisticsDialog::TUsageStatisticsDialog() :
  TCustomDialog(HELP_USAGE)
{
  Caption = LoadStr(USAGE_CAPTION);
  Width = ScaleByTextHeight(this, 444);

  // UnformatMessage is called, because previously, ** markup was used and translations may still contain that
  AddText(CreateLabel(UnformatMessage(LoadStr(USAGE_DATA2))));

  FilterEdit = new TEdit(this);
  FilterEdit->Width = ScaleByTextHeight(this, 277);
  AddEdit(FilterEdit, CreateLabel(LoadStr(USAGE_FILTER)), true);

  UsageMemo = new TMemo(this);
  UsageMemo->Height = ScaleByTextHeight(this, 333);
  UsageMemo->ScrollBars = ssVertical;
  AddEdit(UsageMemo, NULL);
  ReadOnlyControl(UsageMemo);

  ClipboardButton = new TButton(this);
  ClipboardButton->Caption = LoadStr(USAGE_COPY);
  ClipboardButton->Width = ScaleByTextHeight(this, 179);
  ClipboardButton->OnClick = ClipboardButtonClick;
  AddDialogButton(ClipboardButton);

  RemoveCancelButton();
}
//---------------------------------------------------------------------------
void __fastcall TUsageStatisticsDialog::ClipboardButtonClick(TObject * /*Sender*/)
{
  TInstantOperationVisualizer Visualizer;
  CopyToClipboard(UsageMemo->Lines);
}
//---------------------------------------------------------------------------
void __fastcall TUsageStatisticsDialog::DoChange(bool & CanSubmit)
{
  TCustomDialog::DoChange(CanSubmit);
  UnicodeString Text = Configuration->Usage->Serialize(L"\n", FilterEdit->Text);
  bool NoUsage = Text.IsEmpty();
  ClipboardButton->Enabled = !NoUsage;
  if (NoUsage)
  {
    Text = LoadStr(USAGE_DATA_NONE2);
  }
  UsageMemo->Lines->Text = Text;
}
//---------------------------------------------------------------------------
void __fastcall DoUsageStatisticsDialog()
{
  std::unique_ptr<TUsageStatisticsDialog> Dialog(new TUsageStatisticsDialog());
  Dialog->Execute();
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
class TSiteRawDialog : public TCustomDialog
{
public:
  __fastcall TSiteRawDialog();

  bool __fastcall Execute(TSessionData * Data);

protected:
  DYNAMIC void __fastcall DoShow();

private:
  TMemo * SettingsMemo;

  void __fastcall AddButtonClick(TObject * Sender);
  void __fastcall SettingsMemoKeyDown(TObject * Sender, WORD & Key, TShiftState Shift);

  void DeleteNames(TStrings * Names, TStrings * Options);
};
//---------------------------------------------------------------------------
__fastcall TSiteRawDialog::TSiteRawDialog() :
  TCustomDialog(HELP_SITE_RAW)
{
  Caption = LoadStr(SITE_RAW_CAPTION);
  Width = ScaleByTextHeight(this, 444);

  SettingsMemo = new TMemo(this);
  SettingsMemo->Height = ScaleByTextHeight(this, 333);
  SettingsMemo->OnKeyDown = SettingsMemoKeyDown;
  AddEdit(SettingsMemo, NULL);

  TButton * AddButton = new TButton(this);
  AddButton->Caption = LoadStr(SITE_RAW_ADD);
  AddButton->Width = OKButton->Width;
  AddButton->OnClick = AddButtonClick;
  AddDialogButton(AddButton);
}
//---------------------------------------------------------------------------
void __fastcall TSiteRawDialog::DoShow()
{
  TCustomDialog::DoShow();
  InstallPathWordBreakProc(SettingsMemo);
}
//---------------------------------------------------------------------------
bool __fastcall TSiteRawDialog::Execute(TSessionData * Data)
{
  std::unique_ptr<TSessionData> FactoryDefaults(new TSessionData(L""));
  std::unique_ptr<TSessionData> RawData(new TSessionData(L""));
  RawData->Assign(Data);
  // SFTP-only is not reflected by the protocol prefix, we have to use rawsettings for that
  if (RawData->FSProtocol != fsSFTPonly)
  {
    RawData->FSProtocol = FactoryDefaults->FSProtocol;
  }
  RawData->HostName = FactoryDefaults->HostName;
  RawData->PortNumber = FactoryDefaults->PortNumber;
  RawData->UserName = FactoryDefaults->UserName;
  RawData->Password = FactoryDefaults->Password;
  RawData->Ftps = FactoryDefaults->Ftps;

  std::unique_ptr<TStrings> Options(RawData->SaveToOptions(FactoryDefaults.get(), false, false));

  SettingsMemo->Lines = Options.get();

  bool Result = TCustomDialog::Execute();
  if (Result)
  {
    std::unique_ptr<TSessionData> BackupData(new TSessionData(L""));
    BackupData->Assign(Data);
    Data->DefaultSettings();

    Data->FSProtocol = BackupData->FSProtocol;
    Data->HostName = BackupData->HostName;
    Data->PortNumber = BackupData->PortNumber;
    Data->UserName = BackupData->UserName;
    Data->Password = BackupData->Password;
    Data->Ftps = BackupData->Ftps;

    Data->ApplyRawSettings(SettingsMemo->Lines, false);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TSiteRawDialog::SettingsMemoKeyDown(TObject * Sender, WORD & Key, TShiftState Shift)
{
  MemoKeyDown(Sender, Key, Shift);
}
//---------------------------------------------------------------------------
void __fastcall TSiteRawDialog::AddButtonClick(TObject *)
{
  std::unique_ptr<TSessionData> FactoryDefaults(new TSessionData(L""));
  std::unique_ptr<TSessionData> BasicData(new TSessionData(L""));
  BasicData->FSProtocol = TFSProtocol(FactoryDefaults->FSProtocol + 1);
  UnicodeString RandomAppendix(L"_");
  BasicData->HostName = FactoryDefaults->HostName + RandomAppendix;
  BasicData->Ftps = TFtps(FactoryDefaults->Ftps + 1);
  BasicData->PortNumber = BasicData->GetDefaultPort() + 1;
  BasicData->UserName = FactoryDefaults->UserName + RandomAppendix;
  BasicData->Password = FactoryDefaults->Password + RandomAppendix;

  std::unique_ptr<TStrings> BasicOptions(BasicData->SaveToOptions(FactoryDefaults.get(), false, false));

  std::unique_ptr<TStrings> AllOptions(TSessionData::GetAllOptionNames(false));

  std::unique_ptr<TStrings> Names(CreateSortedStringList());
  for (int Index = 0; Index < AllOptions->Count; Index++)
  {
    Names->Add(AllOptions->Names[Index]);
  }
  DeleteNames(Names.get(), BasicOptions.get());
  DeleteNames(Names.get(), SettingsMemo->Lines);

  std::unique_ptr<TCustomDialog> AddDialog(new TCustomDialog(HelpKeyword));
  AddDialog->Caption = LoadStr(SITE_RAW_ADD_CAPTION);
  TComboBox * AddComboBox = new TUIStateAwareComboBox(AddDialog.get());
  AddComboBox->Style = csDropDownList;
  AddComboBox->DropDownCount = Max(AddComboBox->DropDownCount, 16);
  AddDialog->AddComboBox(AddComboBox, CreateLabel(LoadStr(SITE_RAW_ADD_LABEL)), Names.get(), true);
  AddComboBox->ItemIndex = 0;
  if (AddDialog->Execute())
  {
    UnicodeString Name = AddComboBox->Items->Strings[AddComboBox->ItemIndex];
    UnicodeString Value = AllOptions->Values[Name];
    UnicodeString NameAndSeparator = Name + SettingsMemo->Lines->NameValueSeparator;
    int Start = (SettingsMemo->Lines->Text + NameAndSeparator).Length();
    SettingsMemo->Lines->Add(NameAndSeparator + Value);
    SettingsMemo->SetFocus();
    SettingsMemo->SelStart = Start;
    SettingsMemo->SelLength = Value.Length();
  }
}
//---------------------------------------------------------------------------
void TSiteRawDialog::DeleteNames(TStrings * Names, TStrings * Options)
{
  for (int Index = 0; Index < Options->Count; Index++)
  {
    UnicodeString Name = Options->Names[Index];
    if (!Name.IsEmpty())
    {
      int I = Names->IndexOf(Name);
      if (I >= 0)
      {
        Names->Delete(I);
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall DoSiteRawDialog(TSessionData * Data)
{
  std::unique_ptr<TSiteRawDialog> Dialog(new TSiteRawDialog());
  Dialog->Execute(Data);
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
class TSshHostCADialog : public TCustomDialog
{
public:
  TSshHostCADialog(bool Add);
  bool Execute(TSshHostCA & SshHostCA);

protected:
  virtual void __fastcall DoChange(bool & CanSubmit);
  virtual void __fastcall DoValidate();

private:
  TEdit * NameEdit;
  TEdit * PublicKeyEdit;
  TEdit * PublicKeyLabel;
  TEdit * ValidityExpressionEdit;
  TCheckBox * PermitRsaSha1Check;
  TCheckBox * PermitRsaSha256Check;
  TCheckBox * PermitRsaSha512Check;

  void __fastcall BrowseButtonClick(TObject * Sender);
  TCheckBox * AddValidityCheckBox(int CaptionStrPart);
  bool ValidatePublicKey(UnicodeString & Status);
};
//---------------------------------------------------------------------------
TSshHostCADialog::TSshHostCADialog(bool Add) :
  TCustomDialog(HELP_SSH_HOST_CA)
{
  ClientWidth = ScaleByTextHeight(this, 577);
  Caption = LoadStr(Add ? SSH_HOST_CA_ADD : SSH_HOST_CA_EDIT);

  NameEdit = new TEdit(this);
  AddEdit(NameEdit, CreateLabel(LoadStr(SSH_HOST_CA_NAME)));

  PublicKeyEdit = new TEdit(this);
  AddEdit(PublicKeyEdit, CreateLabel(LoadStr(SSH_HOST_CA_PUBLIC_KEY)));

  TButton * BrowseButton = new TButton(this);
  BrowseButton->Caption = LoadStr(SSH_HOST_CA_BROWSE);
  BrowseButton->OnClick = BrowseButtonClick;
  AddButtonNextToEdit(BrowseButton, PublicKeyEdit);
  NameEdit->Width = PublicKeyEdit->Width;

  PublicKeyLabel = new TEdit(this);
  ReadOnlyControl(PublicKeyLabel);
  PublicKeyLabel->BorderStyle = bsNone;
  PublicKeyLabel->TabStop = false;
  AddEditLikeControl(PublicKeyLabel, NULL);

  ValidityExpressionEdit = new TEdit(this);
  AddEdit(ValidityExpressionEdit, CreateLabel(LoadStr(SSH_HOST_CA_PUBLIC_HOSTS)));

  TLabel * Label = CreateLabel(LoadStr(SSH_HOST_CA_SIGNATURE_TYPES));
  AddText(Label);

  PermitRsaSha1Check = AddValidityCheckBox(1);
  int PermitCheckBoxTop = Label->Top - (PermitRsaSha1Check->Height - Label->Height) / 2;
  PermitRsaSha1Check->Left = OKButton->Left + 1;
  PermitRsaSha1Check->Top = PermitCheckBoxTop;
  PermitRsaSha256Check = AddValidityCheckBox(2);
  PermitRsaSha256Check->Left = CancelButton->Left + 1;
  PermitRsaSha256Check->Top = PermitCheckBoxTop;
  PermitRsaSha512Check = AddValidityCheckBox(3);
  PermitRsaSha512Check->Left = HelpButton->Left + 1;
  PermitRsaSha512Check->Top = PermitCheckBoxTop;
}
//---------------------------------------------------------------------------
TCheckBox * TSshHostCADialog::AddValidityCheckBox(int CaptionStrPart)
{
  TCheckBox * Result = new TCheckBox(this);
  Result->Parent = this;
  Result->Caption = LoadStrPart(SSH_HOST_CA_SIGNATURES, CaptionStrPart);
  ScaleButtonControl(Result);
  Result->Left = Left;
  Result->OnClick = Change;
  AddWinControl(Result);
  return Result;
}
//---------------------------------------------------------------------------
bool TSshHostCADialog::ValidatePublicKey(UnicodeString & Status)
{
  bool Result = false;
  UnicodeString PublicKeyText = PublicKeyEdit->Text.Trim();
  if (PublicKeyText.IsEmpty())
  {
    Status = LoadStr(SSH_HOST_CA_NO_KEY);
  }
  else
  {
    RawByteString PublicKeyDummy;
    try
    {
      ParseCertificatePublicKey(PublicKeyText, PublicKeyDummy, Status);
      Result = true;
    }
    catch (Exception & E)
    {
      Status = E.Message;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TSshHostCADialog::DoChange(bool & CanSubmit)
{
  TCustomDialog::DoChange(CanSubmit);

  UnicodeString PublicKeyStatus;
  ValidatePublicKey(PublicKeyStatus);
  // Should drop "SHA256:" prefix, the way we do in TSecureShell::VerifyHostKey
  PublicKeyLabel->Text = PublicKeyStatus;

  CanSubmit = !NameEdit->Text.Trim().IsEmpty() && !PublicKeyEdit->Text.Trim().IsEmpty();
}
//---------------------------------------------------------------------
void __fastcall TSshHostCADialog::DoValidate()
{
  TCustomDialog::DoValidate();

  UnicodeString PublicKeyStatus;
  if (!ValidatePublicKey(PublicKeyStatus))
  {
    PublicKeyEdit->SetFocus();
    throw Exception(MainInstructions(PublicKeyStatus));
  }

  if (ValidityExpressionEdit->Text.Trim().IsEmpty())
  {
    ValidityExpressionEdit->SetFocus();
    throw Exception(MainInstructions(LoadStr(SSH_HOST_CA_NO_HOSTS)));
  }

  UnicodeString Error;
  int ErrorStart, ErrorLen;
  if (!IsCertificateValidityExpressionValid(ValidityExpressionEdit->Text, Error, ErrorStart, ErrorLen))
  {
    std::unique_ptr<TStrings> MoreMessages(TextToStringList(Error));
    MoreMessageDialog(MainInstructions(LoadStr(SSH_HOST_CA_HOSTS_INVALID)), MoreMessages.get(), qtError, qaOK, HelpKeyword);

    ValidityExpressionEdit->SetFocus();
    ValidityExpressionEdit->SelStart = ErrorStart;
    ValidityExpressionEdit->SelLength = ErrorLen;
    Abort();
  }
}
//---------------------------------------------------------------------------
bool TSshHostCADialog::Execute(TSshHostCA & SshHostCA)
{
  NameEdit->Text = SshHostCA.Name;
  RawByteString PublicKey = SshHostCA.PublicKey;
  PublicKeyEdit->Text = EncodeStrToBase64(PublicKey);
  ValidityExpressionEdit->Text = SshHostCA.ValidityExpression;
  PermitRsaSha1Check->Checked = SshHostCA.PermitRsaSha1;
  PermitRsaSha256Check->Checked = SshHostCA.PermitRsaSha256;
  PermitRsaSha512Check->Checked = SshHostCA.PermitRsaSha512;
  bool Result = TCustomDialog::Execute();
  if (Result)
  {
    SshHostCA.Name = NameEdit->Text;
    SshHostCA.PublicKey = DecodeBase64ToStr(PublicKeyEdit->Text);
    SshHostCA.ValidityExpression = ValidityExpressionEdit->Text;
    SshHostCA.PermitRsaSha1 = PermitRsaSha1Check->Checked;
    SshHostCA.PermitRsaSha256 = PermitRsaSha256Check->Checked;
    SshHostCA.PermitRsaSha512 = PermitRsaSha512Check->Checked;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TSshHostCADialog::BrowseButtonClick(TObject *)
{
  std::unique_ptr<TOpenDialog> OpenDialog(new TOpenDialog(Application));
  OpenDialog->Title = LoadStr(SSH_HOST_CA_BROWSE_TITLE);
  OpenDialog->Filter = LoadStr(SSH_HOST_CA_BROWSE_FILTER);
  bool Result = OpenDialog->Execute();
  if (Result)
  {
    UnicodeString FileName = OpenDialog->FileName;
    UnicodeString Algorithm, Comment;
    bool HasCertificate;
    RawByteString PublicKey;
    try
    {
      PublicKey = LoadPublicKey(FileName, Algorithm, Comment, HasCertificate);
    }
    catch (Exception & E)
    {
      throw ExtException(&E, MainInstructions(FMTLOAD(SSH_HOST_CA_LOAD_ERROR, (FileName))));
    }

    if (NameEdit->Text.IsEmpty())
    {
      NameEdit->Text = Comment;
    }
    PublicKeyEdit->Text = EncodeStrToBase64(PublicKey);
  }
}
//---------------------------------------------------------------------------
bool DoSshHostCADialog(bool Add, TSshHostCA & SshHostCA)
{
  std::unique_ptr<TSshHostCADialog> Dialog(new TSshHostCADialog(Add));
  return Dialog->Execute(SshHostCA);
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
class TTagDialog : public TCustomDialog
{
public:
  TTagDialog(bool Add, TStrings * Tags);
  bool Execute(UnicodeString & Key, UnicodeString & Value);

protected:
  virtual void __fastcall DoChange(bool & CanSubmit);
  virtual void __fastcall DoValidate();

private:
  TEdit * KeyEdit;
  TEdit * ValueEdit;
  TStrings * FTags;

  void ValidateTag(TEdit * Edit);
};
//---------------------------------------------------------------------------
TTagDialog::TTagDialog(bool Add, TStrings * Tags) :
  TCustomDialog(HELP_TAG)
{
  Caption = LoadStr(Add ? TAG_ADD : TAG_EDIT);
  FTags = Tags;

  KeyEdit = new TEdit(this);
  KeyEdit->MaxLength = 128;
  AddEdit(KeyEdit, CreateLabel(LoadStr(TAG_KEY)));

  ValueEdit = new TEdit(this);
  ValueEdit->MaxLength = 256;
  AddEdit(ValueEdit, CreateLabel(LoadStr(TAG_VALUE)));
}
//---------------------------------------------------------------------------
bool TTagDialog::Execute(UnicodeString & Key, UnicodeString & Value)
{
  // if we happen to get values longer than what we believed was possible, allow them.
  KeyEdit->MaxLength = std::max(KeyEdit->MaxLength, Key.Length());
  KeyEdit->Text = Key;
  ValueEdit->MaxLength = std::max(ValueEdit->MaxLength, Value.Length());
  ValueEdit->Text = Value;
  bool Result = TCustomDialog::Execute();
  if (Result)
  {
    Key = KeyEdit->Text;
    Value = ValueEdit->Text;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTagDialog::DoChange(bool & CanSubmit)
{
  CanSubmit = !KeyEdit->Text.IsEmpty();
}
//---------------------------------------------------------------------------
void TTagDialog::ValidateTag(TEdit * Edit)
{
  // there are lot more in various scripts
  UnicodeString InvalidChars = L"!\"#$%&'()*,;<>?[\\]^`{|}~¡¢£¤¥¦§¨©«¬­®¯°±´¶·¸»¿×÷";
  UnicodeString Text = Edit->Text;
  for (int Index = 1; Index <= Text.Length(); Index++)
  {
    wchar_t Ch = Text[Index];
    if (TCharacter::IsControl(Ch) ||
        (InvalidChars.Pos(Ch) > 0))
    {
      UnicodeString Message = MainInstructions(FMTLOAD(TAG_INVALID_CHAR, (Ch)));
      if (MoreMessageDialog(Message, NULL, qtWarning, qaIgnore | qaAbort, HelpKeyword) != qaIgnore)
      {
        Edit->SetFocus();
        Abort();
      }
      else
      {
        break;
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTagDialog::DoValidate()
{
  UnicodeString Key = KeyEdit->Text;
  if (FTags->IndexOf(Key) >= 0)
  {
    throw Exception(MainInstructions(LoadStr(TAG_NOT_UNIQUE)));
  }

  ValidateTag(KeyEdit);
  ValidateTag(ValueEdit);

  TCustomDialog::DoValidate();
}
//---------------------------------------------------------------------------
bool DoTagDialog(bool Add, TStrings * Tags, UnicodeString & Key, UnicodeString & Value)
{
  std::unique_ptr<TTagDialog> Dialog(new TTagDialog(Add, Tags));
  return Dialog->Execute(Key, Value);
}
