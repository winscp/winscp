//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>

#include "ImportSessions.h"

#include <Configuration.h>
#include <CoreMain.h>

#include <VCLCommon.h>
#include <WinInterface.h>
#include <TextsWin.h>
#include <CoreMain.h>
//---------------------------------------------------------------------
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------
bool __fastcall DoImportSessionsDialog()
{
  std::auto_ptr<TStoredSessionList> PuttyImportSessionList(
    GUIConfiguration->SelectPuttySessionsForImport(StoredSessions));
  std::auto_ptr<TStoredSessionList> FilezillaImportSessionList(
    GUIConfiguration->SelectFilezillaSessionsForImport(StoredSessions));

  std::auto_ptr<TList> SessionListsList(new TList());
  SessionListsList->Add(PuttyImportSessionList.get());
  SessionListsList->Add(FilezillaImportSessionList.get());

  bool ImportKeys = true;
  std::auto_ptr<TImportSessionsDialog> ImportSessionsDialog(
    new TImportSessionsDialog(Application, SessionListsList.get()));

  bool Result = ImportSessionsDialog->Execute(ImportKeys);

  if (Result)
  {
    StoredSessions->Import(PuttyImportSessionList.get(), true);
    StoredSessions->Import(FilezillaImportSessionList.get(), true);

    if (ImportKeys)
    {
      UnicodeString TargetKey = Configuration->RegistryStorageKey + L"\\" + Configuration->SshHostKeysSubKey;
      UnicodeString SourceKey = Configuration->PuttyRegistryStorageKey + L"\\" + Configuration->SshHostKeysSubKey;

      TStoredSessionList::ImportHostKeys(TargetKey, SourceKey, PuttyImportSessionList.get(), true);

      // Filezilla uses PuTTY's host key store
      TStoredSessionList::ImportHostKeys(TargetKey, SourceKey, FilezillaImportSessionList.get(), true);
    }
  }
  return Result;
}
//---------------------------------------------------------------------
__fastcall TImportSessionsDialog::TImportSessionsDialog(TComponent * AOwner,
  TList * SessionListsList)
  : TForm(AOwner)
{
  UseSystemSettings(this);
  // this is loaded from res string to force translation
  Caption = LoadStr(IMPORT_CAPTION);

  for (int Index = 0; Index < SessionListsList->Count; Index++)
  {
    SourceComboBox->Items->Objects[Index] = static_cast<TObject *>(SessionListsList->Items[Index]);
    if ((SourceComboBox->ItemIndex < 0) && (GetSessionList(Index)->Count > 0))
    {
      SourceComboBox->ItemIndex = Index;
    }
  }

  // should not happen as we never get here when there are no session to import
  if (SourceComboBox->ItemIndex < 0)
  {
    SourceComboBox->ItemIndex = 0;
  }

  LoadSessions();
}
//---------------------------------------------------------------------
TStoredSessionList * __fastcall TImportSessionsDialog::GetSessionList(int Index)
{
  return dynamic_cast<TStoredSessionList *>(SourceComboBox->Items->Objects[Index]);
}
//---------------------------------------------------------------------
void __fastcall TImportSessionsDialog::UpdateControls()
{
  EnableControl(OKButton, ListViewAnyChecked(SessionListView2));

  bool AnySshChecked = false;
  for (int Index = 0; Index < SessionListView2->Items->Count; Index++)
  {
    TListItem * Item = SessionListView2->Items->Item[Index];
    TSessionData * Data = (TSessionData*)Item->Data;
    if (Item->Checked && Data->UsesSsh)
    {
      AnySshChecked = true;
      break;
    }
  }

  EnableControl(ImportKeysCheck, AnySshChecked);

  EnableControl(CheckAllButton, SessionListView2->Items->Count > 0);
  AdjustListColumnsWidth(SessionListView2);
}
//---------------------------------------------------------------------
void __fastcall TImportSessionsDialog::ClearSelections()
{
  for (int Index = 0; Index < SourceComboBox->Items->Count; Index++)
  {
    TStoredSessionList * SessionList = GetSessionList(Index);
    for (int Index = 0; Index < SessionList->Count; Index++)
    {
      SessionList->Sessions[Index]->Selected = false;
    }
  }
}
//---------------------------------------------------------------------
void __fastcall TImportSessionsDialog::SaveSelection()
{
  for (int Index = 0; Index < SessionListView2->Items->Count; Index++)
  {
    ((TSessionData*)SessionListView2->Items->Item[Index]->Data)->Selected =
      SessionListView2->Items->Item[Index]->Checked;
  }
}
//---------------------------------------------------------------------
void __fastcall TImportSessionsDialog::LoadSessions()
{
  SessionListView2->Items->BeginUpdate();
  try
  {
    SessionListView2->Items->Clear();
    TStoredSessionList * SessionList = GetSessionList(SourceComboBox->ItemIndex);
    for (int Index = 0; Index < SessionList->Count; Index++)
    {
      TSessionData * Session = SessionList->Sessions[Index];
      TListItem * Item = SessionListView2->Items->Add();
      Item->Data = Session;
      Item->Caption = Session->Name;
      Item->Checked = Session->Selected;
    }
  }
  __finally
  {
    SessionListView2->Items->EndUpdate();
  }
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TImportSessionsDialog::SessionListView2InfoTip(
      TObject * /*Sender*/, TListItem * Item, UnicodeString & InfoTip)
{
  InfoTip = ((TSessionData*)Item->Data)->InfoTip;
}
//---------------------------------------------------------------------------
void __fastcall TImportSessionsDialog::SessionListView2MouseDown(
      TObject * /*Sender*/, TMouseButton /*Button*/, TShiftState /*Shift*/,
      int /*X*/, int /*Y*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TImportSessionsDialog::SessionListView2KeyUp(
      TObject * /*Sender*/, WORD & /*Key*/, TShiftState /*Shift*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TImportSessionsDialog::FormShow(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TImportSessionsDialog::CheckAllButtonClick(TObject * /*Sender*/)
{
  ListViewCheckAll(SessionListView2, caToggle);
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TImportSessionsDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
bool __fastcall TImportSessionsDialog::Execute(bool & ImportKeys)
{
  ImportKeysCheck->Checked = ImportKeys;

  bool Result = (ShowModal() == mrOk);

  if (Result)
  {
    ClearSelections();
    SaveSelection();
    ImportKeys = ImportKeysCheck->Enabled && ImportKeysCheck->Checked;
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TImportSessionsDialog::SourceComboBoxSelect(TObject * /*Sender*/)
{
  SaveSelection();
  LoadSessions();
}
//---------------------------------------------------------------------------
