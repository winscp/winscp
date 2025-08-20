//---------------------------------------------------------------------------
#include <FormsPCH.h>
#pragma hdrstop

#include "EditorPreferences.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "HistoryComboBox"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
bool __fastcall DoEditorPreferencesDialog(TEditorData * Editor,
  bool & Remember, TEditorPreferencesMode Mode, bool MayRemote)
{
  bool Result;

  TEditorPreferencesDialog * Dialog = SafeFormCreate<TEditorPreferencesDialog>();
  try
  {
    Dialog->Init(Mode, MayRemote);
    Result = Dialog->Execute(Editor, Remember);
  }
  __finally
  {
    delete Dialog;
  }
  return Result;
}
//---------------------------------------------------------------------------
__fastcall TEditorPreferencesDialog::TEditorPreferencesDialog(
  TComponent * Owner) :
  TForm(Owner)
{
  SetCorrectFormParent(this);
  UseSystemSettings(this);

  UnicodeString ExecutableDescription;
  UnicodeString UsageState;
  bool TryNextTime;
  if (DetectSystemExternalEditor(true, FSystemExternalEditor, ExecutableDescription, UsageState, TryNextTime))
  {
    FSystemExternalEditor = FormatCommand(FSystemExternalEditor, L"");
  }
}
//---------------------------------------------------------------------------
void __fastcall TEditorPreferencesDialog::Init(TEditorPreferencesMode Mode, bool MayRemote)
{
  int CaptionId;
  switch (Mode)
  {
    case epmEdit:
      CaptionId = EDITOR_EDIT;
      break;

    case epmAdd:
      CaptionId = EDITOR_ADD;
      break;

    case epmAdHoc:
      CaptionId = EDITOR_AD_HOC;
      break;
  }

  Caption = LoadStr(CaptionId);

  if (Mode == epmAdHoc)
  {
    int Shift = ExternalEditorEdit->Left - MaskEdit->Left;
    ExternalEditorEdit->Left = MaskEdit->Left;
    ExternalEditorEdit->Width = ExternalEditorEdit->Width + Shift;
    Shift = ExternalEditorEdit->Top - MaskEdit->Top;
    ExternalEditorEdit->Top = MaskEdit->Top;
    ExternalEditorBrowseButton->Top = ExternalEditorBrowseButton->Top - Shift;
    Shift += (EditorExternalButton->Top - EditorInternalButton->Top);
    EditorGroup2->Height = EditorGroup2->Height - Shift;
    TLabel * ExternalEditorLabel = CreateLabel(this);
    ExternalEditorLabel->Caption = EditorExternalButton->Caption;
    ExternalEditorLabel->Parent = EditorGroup2;
    ExternalEditorLabel->Top = MaskLabel->Top;
    ExternalEditorLabel->Left = MaskLabel->Left;
    ExternalEditorLabel->FocusControl = ExternalEditorEdit;
    EditorInternalButton->Visible = false;
    EditorExternalButton->Visible = false;
    EditorOpenButton->Visible = false;
    DefaultButton->Top = DefaultButton->Top - Shift;
    DefaultButton->Left = ExternalEditorEdit->Left;
    Shift += ExternalEditorGroup->Top - MaskGroup->Top;
    MaskGroup->Visible = false;
    ExternalEditorGroup->Top = ExternalEditorGroup->Top - Shift;
    Height = Height - Shift;
  }
  else
  {
    int Shift = OkButton->Top - RememberCheck->Top;
    RememberCheck->Visible = false;
    Height = Height - Shift;
  }

  FMayRemote = MayRemote;
}
//---------------------------------------------------------------------------
bool __fastcall TEditorPreferencesDialog::Execute(TEditorData * Editor, bool & Remember)
{
  EditorInternalButton->Checked = (Editor->Editor == edInternal);
  EditorExternalButton->Checked = (Editor->Editor == edExternal);
  EditorOpenButton->Checked = (Editor->Editor == edOpen);
  UnicodeString ExternalEditor = Editor->ExternalEditor;
  if (!ExternalEditor.IsEmpty())
  {
    ReformatFileNameCommand(ExternalEditor);
  }
  ExternalEditorEdit->Text = ExternalEditor;
  ExternalEditorEdit->Items = CustomWinConfiguration->History[L"ExternalEditor"];
  FExternalEditorDefaults = GetExternalEditorDefaults();
  MaskEdit->Text = Editor->FileMask.Masks;
  MaskEdit->Items = CustomWinConfiguration->History[L"Mask"];
  ExternalEditorTextCheck->Checked = Editor->ExternalEditorText;
  SDIExternalEditorCheck->Checked = Editor->SDIExternalEditor;
  RememberCheck->Checked = Remember;
  UpdateControls();

  bool Result = (ShowModal() == DefaultResult(this));

  if (Result)
  {
    if (EditorExternalButton->Checked)
    {
      Editor->Editor = edExternal;
    }
    else if (EditorOpenButton->Checked)
    {
      Editor->Editor = edOpen;
    }
    else
    {
      Editor->Editor = edInternal;
    }
    Editor->ExternalEditor = ExternalEditorEdit->Text;
    ExternalEditorEdit->SaveToHistory();
    CustomWinConfiguration->History[L"ExternalEditor"] = ExternalEditorEdit->Items;
    Editor->FileMask = MaskEdit->Text;
    MaskEdit->SaveToHistory();
    CustomWinConfiguration->History[L"Mask"] = MaskEdit->Items;
    Editor->ExternalEditorText = ExternalEditorTextCheck->Checked;
    Editor->SDIExternalEditor = SDIExternalEditorCheck->Checked;
    Remember = RememberCheck->Checked;
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TEditorPreferencesDialog::ExternalEditorEditExit(
  TObject * Sender)
{
  // duplicated in TPreferencesDialog::FilenameEditExit
  THistoryComboBox * FilenameEdit = dynamic_cast<THistoryComboBox *>(Sender);
  try
  {
    UnicodeString Filename = FilenameEdit->Text;
    if (!Filename.IsEmpty())
    {
      ReformatFileNameCommand(Filename);
      FilenameEdit->Text = Filename;
    }
    ControlChange(Sender);
  }
  catch(...)
  {
    FilenameEdit->SelectAll();
    FilenameEdit->SetFocus();
    throw;
  }

  ExternalEditorOptionsAutodetect();
}
//---------------------------------------------------------------------------
TEditorData __fastcall TEditorPreferencesDialog::GetExternalEditorDefaults()
{
  TEditorData Result;
  Result.ExternalEditor = ExternalEditorEdit->Text;
  Result.ExternalEditorOptionsAutodetect();
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TEditorPreferencesDialog::ExternalEditorOptionsAutodetect()
{
  DebugAssert(EditorExternalButton->Checked);
  TEditorData Defaults = GetExternalEditorDefaults();

  if (!(FExternalEditorDefaults == Defaults))
  {
    ExternalEditorTextCheck->Checked = Defaults.ExternalEditorText;
    SDIExternalEditorCheck->Checked = Defaults.SDIExternalEditor;
    FExternalEditorDefaults = Defaults;
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TEditorPreferencesDialog::ExternalEditorBrowseButtonClick(
  TObject * /*Sender*/)
{
  BrowseForExecutable(ExternalEditorEdit,
    LoadStr(PREFERENCES_SELECT_EXTERNAL_EDITOR),
    LoadStr(EXECUTABLE_FILTER), true, false);
  ExternalEditorOptionsAutodetect();
}
//---------------------------------------------------------------------------
void __fastcall TEditorPreferencesDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
void __fastcall TEditorPreferencesDialog::ControlChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TEditorPreferencesDialog::UpdateControls()
{
  EnableControl(OkButton,
    EditorInternalButton->Checked || EditorOpenButton->Checked ||
    !ExternalEditorEdit->Text.IsEmpty());
  EnableControl(ExternalEditorEdit, EditorExternalButton->Checked);
  EnableControl(ExternalEditorBrowseButton, EditorExternalButton->Checked);
  EnableControl(ExternalEditorGroup, EditorExternalButton->Checked && FMayRemote);
  EnableControl(DefaultButton, !FSystemExternalEditor.IsEmpty());
}
//---------------------------------------------------------------------------
void __fastcall TEditorPreferencesDialog::FormCloseQuery(TObject * /*Sender*/,
  bool & /*CanClose*/)
{
  if (ModalResult == DefaultResult(this))
  {
    ExitActiveControl(this);
  }
}
//---------------------------------------------------------------------------
void __fastcall TEditorPreferencesDialog::MaskEditExit(TObject * /*Sender*/)
{
  ValidateMaskEdit(MaskEdit);
}
//---------------------------------------------------------------------------
void __fastcall TEditorPreferencesDialog::FormShow(TObject * /*Sender*/)
{
  InstallPathWordBreakProc(ExternalEditorEdit);
}
//---------------------------------------------------------------------------
void __fastcall TEditorPreferencesDialog::DefaultButtonClick(TObject * /*Sender*/)
{
  EditorExternalButton->Checked = true;
  ExternalEditorEdit->Text = FSystemExternalEditor;
  UpdateControls();
  ExternalEditorOptionsAutodetect();
}
//---------------------------------------------------------------------------
