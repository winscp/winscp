//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Dialogs.hpp>
//---------------------------------------------------------------------
#include <Common.h>
#include <TextsWin.h>
#include <WinInterface.h>
#include <VCLCommon.h>

#include "SaveSession.h"
//---------------------------------------------------------------------
#pragma resource "*.dfm"
//---------------------------------------------------------------------
AnsiString __fastcall DoSaveSessionDialog(
  TStoredSessionList *SessionList, const AnsiString DefaultName)
{
  AnsiString Result;
  TSaveSessionDialog *SaveSessionDialog = new TSaveSessionDialog(Application);
  try {
    SaveSessionDialog->SessionList = SessionList;
    SaveSessionDialog->SessionName = DefaultName;
    if (SaveSessionDialog->ShowModal() == mrOk)
        Result = SaveSessionDialog->SessionName;
      else Result = "";
  } __finally {
    delete SaveSessionDialog;
  }
  return Result;
}
//---------------------------------------------------------------------
__fastcall TSaveSessionDialog::TSaveSessionDialog(TComponent* AOwner)
	: TForm(AOwner)
{
  UseSystemFont(this);
}
//---------------------------------------------------------------------
void __fastcall TSaveSessionDialog::SetSessionList(TStoredSessionList * value)
{
  if (FSessionList != value)
  {
    FSessionList = value;
    SessionNameBox->Items->BeginUpdate();
    try {
      SessionNameBox->Items->Clear();
      for (Integer Index = 0; Index < SessionList->Count; Index++)
      {
        TSessionData * Data = SessionList->Sessions[Index];
        if (!Data->Special) SessionNameBox->Items->Add(Data->Name);
      }
    }
    __finally {
      SessionNameBox->Items->EndUpdate();
      UpdateControls();
    }
  }
}
//---------------------------------------------------------------------
void __fastcall TSaveSessionDialog::SetSessionName(AnsiString value)
{
  SessionNameBox->Text = value;
  UpdateControls();
}
//---------------------------------------------------------------------
AnsiString __fastcall TSaveSessionDialog::GetSessionName()
{
  return SessionNameBox->Text;
}
//---------------------------------------------------------------------------
void __fastcall TSaveSessionDialog::SessionNameBoxChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TSaveSessionDialog::UpdateControls()
{
  EnableControl(OKButton, !SessionName.IsEmpty());
}
//---------------------------------------------------------------------------
void __fastcall TSaveSessionDialog::FormCloseQuery(TObject * /*Sender*/,
      bool &CanClose)
{
  CanClose = true;
  if (ModalResult == mrOk)
  {
    TSessionData * Data = (TSessionData *)SessionList->FindByName(SessionName);
    if (Data && Data->Special)
    {
      MessageDialog(FMTLOAD(CANNOT_OVERWRITE_SPECIAL_SESSION, (SessionName)),
        qtError, qaOK, 0);
      CanClose = false;
    }
    else if (Data &&
      MessageDialog(FMTLOAD(CONFIRM_OVERWRITE_SESSION, (SessionName)),
        qtConfirmation, qaYes | qaNo, 0) != qaYes)
    {
      CanClose = false;
    }
  }
}
//---------------------------------------------------------------------------
