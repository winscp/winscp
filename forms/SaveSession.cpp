//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Dialogs.hpp>
//---------------------------------------------------------------------
#include <Common.h>
#include <HelpWin.h>
#include <WinInterface.h>
#include <VCLCommon.h>
#include <CoreMain.h>
#include <TextsWin.h>

#include "SaveSession.h"
//---------------------------------------------------------------------
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------------
bool __fastcall DoSaveSessionDialog(AnsiString & SessionName,
  bool * SavePassword, TSessionData * OriginalSession)
{
  bool Result;
  TSaveSessionDialog * Dialog = NULL;
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

    Dialog = new TSaveSessionDialog(Application, Items, OriginalSession,
      (SavePassword != NULL));
    bool ASavePassword = ((SavePassword != NULL) ? *SavePassword : false);
    Result = Dialog->Execute(SessionName, ASavePassword);
    if (Result)
    {
      if (SavePassword != NULL)
      {
        *SavePassword = ASavePassword;
      }
    }
  }
  __finally
  {
    delete Dialog;
    delete Items;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall SessionNameValidate(const AnsiString & Text,
  TSessionData * RenamingSession)
{
  TSessionData::ValidatePath(Text);

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
__fastcall TSaveSessionDialog::TSaveSessionDialog(TComponent * AOwner,
    TStrings * Items, TSessionData * OriginalSession, bool AllowSavePassword) :
  TForm(AOwner),
  FOriginalSession(OriginalSession)
{
  UseSystemSettings(this);
  InstallPathWordBreakProc(InputCombo);
  InputCombo->Items = Items;
  EnableControl(SavePasswordCheck, AllowSavePassword);
}
//---------------------------------------------------------------------
bool __fastcall TSaveSessionDialog::Execute(AnsiString & SessionName, bool & SavePassword)
{
  InputCombo->Text = SessionName;
  SavePasswordCheck->Checked = SavePassword;
  bool Result = (ShowModal() == mrOk);
  if (Result)
  {
    SessionName = InputCombo->Text;
    SavePassword = SavePasswordCheck->Checked;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TSaveSessionDialog::InputComboChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TSaveSessionDialog::UpdateControls()
{
  EnableControl(OKButton, !InputCombo->Text.IsEmpty());
}
//---------------------------------------------------------------------------
void __fastcall TSaveSessionDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
void __fastcall TSaveSessionDialog::FormCloseQuery(TObject * /*Sender*/,
  bool & /*CanClose*/)
{
  if (ModalResult == mrOk)
  {
    SessionNameValidate(InputCombo->Text, FOriginalSession);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSaveSessionDialog::FormShow(TObject * /*Sender*/)
{
  int P = InputCombo->Text.LastDelimiter("/");
  if (P > 0)
  {
    InputCombo->SetFocus();
    InputCombo->SelStart = P;
    InputCombo->SelLength = InputCombo->Text.Length() - P;
  }
  UpdateControls();
}
//---------------------------------------------------------------------------
