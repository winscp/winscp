//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Dialogs.hpp>
//---------------------------------------------------------------------
#include <Common.h>
#include <TextsWin.h>
#include <WinInterface.h>
#include <VCLCommon.h>
#include <ScpMain.h>

#include "ComboInput.h"
//---------------------------------------------------------------------
#pragma resource "*.dfm"
//---------------------------------------------------------------------
bool __fastcall DoComboInputDialog(
  const AnsiString Caption, const AnsiString Prompt,
  AnsiString & Text, TStrings * Items, TCloseQueryEvent OnCloseQuery,
  bool AllowEmpty)
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
    ComboInputDialog->OnCloseQuery = OnCloseQuery;
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
AnsiString __fastcall DoSaveSessionDialog(
  TStoredSessionList * ASessionList, const AnsiString DefaultName)
{
  // SessionList is no longer passed to TComboInputDialog, it uses global variable
  assert(ASessionList == StoredSessions);
  
  AnsiString Result;
  TComboInputDialog * SaveSessionDialog = NULL;
  TStrings * Items = NULL;
  try
  {
    Items = new TStringList();
    for (int Index = 0; Index < ASessionList->Count; Index++)
    {
      TSessionData * Data = ASessionList->Sessions[Index];
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
    SaveSessionDialog->OnCloseQuery = SaveSessionDialog->StoredSessionsCloseQuery;
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
//---------------------------------------------------------------------
__fastcall TComboInputDialog::TComboInputDialog(TComponent* AOwner)
	: TForm(AOwner)
{
  FAllowEmpty = false;
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
void __fastcall TComboInputDialog::StoredSessionsCloseQuery(TObject * /*Sender*/,
      bool & CanClose)
{
  CanClose = true;
  if (ModalResult == mrOk)
  {
    assert(StoredSessions);
    TSessionData * Data = (TSessionData *)StoredSessions->FindByName(Text);
    if (Data && Data->Special)
    {
      MessageDialog(FMTLOAD(CANNOT_OVERWRITE_SPECIAL_SESSION, (Text)),
        qtError, qaOK, 0);
      CanClose = false;
    }
    else if (Data &&
      MessageDialog(FMTLOAD(CONFIRM_OVERWRITE_SESSION, (Text)),
        qtConfirmation, qaYes | qaNo, 0) != qaYes)
    {
      CanClose = false;
    }
  }
}
//---------------------------------------------------------------------------
