//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Dialogs.hpp>
//---------------------------------------------------------------------
#include <Common.h>
#include <TextsWin.h>
#include <HelpWin.h>
#include <WinInterface.h>
#include <VCLCommon.h>
#include <CoreMain.h>

#include "ComboInput.h"
//---------------------------------------------------------------------
#pragma resource "*.dfm"
//---------------------------------------------------------------------
bool __fastcall DoComboInputDialog(
  const AnsiString Caption, const AnsiString Prompt,
  AnsiString & Text, TStrings * Items, TInputValidateEvent OnInputValidate,
  bool AllowEmpty, const AnsiString HelpKeyword)
{
  bool Result;
  TComboInputDialog * ComboInputDialog = new TComboInputDialog(Application);
  try
  {
    ComboInputDialog->Caption = Caption;
    ComboInputDialog->Prompt = Prompt;
    ComboInputDialog->Text = Text;
    ComboInputDialog->Items = Items;
    ComboInputDialog->AllowEmpty = AllowEmpty;
    ComboInputDialog->OnInputValidate = OnInputValidate;
    ComboInputDialog->HelpKeyword = HelpKeyword;
    Result = (ComboInputDialog->ShowModal() == mrOk);
    if (Result)
    {
      Text = ComboInputDialog->Text;
    }
  }
  __finally
  {
    delete ComboInputDialog;
  }
  return Result;
}
//---------------------------------------------------------------------
void __fastcall SaveSessionInputValidate(TObject * Sender, const AnsiString & Text)
{
  TSessionData * Data =
    reinterpret_cast<TSessionData *>(dynamic_cast<TForm *>(Sender)->Tag);
  SessionNameValidate(Text, Data);
}
//---------------------------------------------------------------------
AnsiString __fastcall DoSaveSessionDialog(const AnsiString DefaultName,
  TSessionData * OriginalSession)
{
  AnsiString Result;
  TComboInputDialog * SaveSessionDialog = NULL;
  TStrings * Items = NULL;
  try
  {
    Items = new TStringList();
    for (int Index = 0; Index < StoredSessions->Count; Index++)
    {
      TSessionData * Data = StoredSessions->Sessions[Index];
      if (!Data->Special)
      {
        Items->Add(Data->Name);
      }
    }

    SaveSessionDialog = new TComboInputDialog(Application);
    SaveSessionDialog->Items = Items;
    SaveSessionDialog->Text = DefaultName;
    SaveSessionDialog->Caption = LoadStr(SAVE_SESSION_CAPTION);
    SaveSessionDialog->Prompt = LoadStr(SAVE_SESSION_PROMPT);
    SaveSessionDialog->OnInputValidate = SaveSessionInputValidate;
    SaveSessionDialog->Tag = reinterpret_cast<int>(OriginalSession);
    SaveSessionDialog->HelpKeyword = HELP_SESSION_SAVE;
    if (SaveSessionDialog->ShowModal() == mrOk)
    {
      Result = SaveSessionDialog->Text;
    }
  }
  __finally
  {
    delete SaveSessionDialog;
    delete Items;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall SessionNameValidate(const AnsiString & Text,
  TSessionData * RenamingSession)
{
  TSessionData::ValidateName(Text);

  assert(StoredSessions);
  TSessionData * Data = (TSessionData *)StoredSessions->FindByName(Text);
  if (Data && Data->Special)
  {
    MessageDialog(FMTLOAD(CANNOT_OVERWRITE_SPECIAL_SESSION, (Text)),
      qtError, qaOK, HELP_NONE);
    Abort();
  }
  else if (Data && (Data != RenamingSession) &&
    MessageDialog(FMTLOAD(CONFIRM_OVERWRITE_SESSION, (Text)),
      qtConfirmation, qaYes | qaNo, HELP_SESSION_SAVE_OVERWRITE) != qaYes)
  {
    Abort();
  }
}
//---------------------------------------------------------------------
__fastcall TComboInputDialog::TComboInputDialog(TComponent* AOwner)
  : TForm(AOwner)
{
  FAllowEmpty = false;
  FOnInputValidate = NULL;
  UseSystemSettings(this);
}
//---------------------------------------------------------------------
void __fastcall TComboInputDialog::SetItems(TStrings * value)
{
  InputCombo->Items = value;
  UpdateControls();
}
//---------------------------------------------------------------------
TStrings * __fastcall TComboInputDialog::GetItems()
{
  return InputCombo->Items;
}
//---------------------------------------------------------------------
void __fastcall TComboInputDialog::SetText(AnsiString value)
{
  InputCombo->Text = value;
  UpdateControls();
}
//---------------------------------------------------------------------
AnsiString __fastcall TComboInputDialog::GetText()
{
  return InputCombo->Text;
}
//---------------------------------------------------------------------------
void __fastcall TComboInputDialog::SetPrompt(AnsiString value)
{
  InputLabel->Caption = value;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TComboInputDialog::GetPrompt()
{
  return InputLabel->Caption;
}
//---------------------------------------------------------------------------
void __fastcall TComboInputDialog::InputComboChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TComboInputDialog::UpdateControls()
{
  EnableControl(OKButton, !Text.IsEmpty() || FAllowEmpty);
}
//---------------------------------------------------------------------------
void __fastcall TComboInputDialog::FormShow(TObject * /*Sender*/)
{
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
//---------------------------------------------------------------------------
void __fastcall TComboInputDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
void __fastcall TComboInputDialog::FormCloseQuery(TObject * /*Sender*/,
  bool & /*CanClose*/)
{
  if ((ModalResult == mrOk) && (OnInputValidate != NULL))
  {
    OnInputValidate(this, Text);
  }
}
//---------------------------------------------------------------------------
