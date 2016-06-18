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
#include <PasTools.hpp>
//---------------------------------------------------------------------
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------
bool __fastcall DoImportSessionsDialog(TList * Imported)
{
  std::unique_ptr<TStrings> Errors(new TStringList());
  UnicodeString Error;
  std::unique_ptr<TStoredSessionList> PuttyImportSessionList(
    GUIConfiguration->SelectPuttySessionsForImport(StoredSessions, Error));
  Errors->Add(Error);
  std::unique_ptr<TStoredSessionList> FilezillaImportSessionList(
    Configuration->SelectFilezillaSessionsForImport(StoredSessions, Error));
  Errors->Add(Error);

  std::unique_ptr<TList> SessionListsList(new TList());
  SessionListsList->Add(PuttyImportSessionList.get());
  SessionListsList->Add(FilezillaImportSessionList.get());

  bool ImportKeys = true;

  std::unique_ptr<TImportSessionsDialog> ImportSessionsDialog(
    SafeFormCreate<TImportSessionsDialog>(Application));

  ImportSessionsDialog->Init(SessionListsList.get(), Errors.get());

  bool Result = ImportSessionsDialog->Execute(ImportKeys);

  if (Result)
  {
    StoredSessions->Import(PuttyImportSessionList.get(), true, Imported);
    StoredSessions->Import(FilezillaImportSessionList.get(), true, Imported);

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
__fastcall TImportSessionsDialog::TImportSessionsDialog(TComponent * AOwner) :
  TForm(AOwner)
{
  UseSystemSettings(this);
  // this is loaded from res string to force translation
  Caption = LoadStr(IMPORT_CAPTION);
}
//---------------------------------------------------------------------
void __fastcall TImportSessionsDialog::Init(TList * SessionListsList, TStrings * Errors)
{
  FErrors = Errors;

  for (int Index = 0; Index < SessionListsList->Count; Index++)
  {
    SourceComboBox->Items->Objects[Index] = static_cast<TObject *>(SessionListsList->Items[Index]);
    if ((SourceComboBox->ItemIndex < 0) && (GetSessionList(Index)->Count > 0))
    {
      SourceComboBox->ItemIndex = Index;
    }
  }

  if (SourceComboBox->ItemIndex < 0)
  {
    SourceComboBox->ItemIndex = 0;
  }

  LoadSessions();

  int Offset = ScaleByTextHeight(this, 8);
  ErrorPanel->BoundsRect =
    TRect(
      SessionListView2->BoundsRect.Left + Offset, SessionListView2->BoundsRect.Top + Offset,
      SessionListView2->BoundsRect.Right - Offset, SessionListView2->BoundsRect.Bottom - Offset);

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
  AutoSizeListColumnsWidth(SessionListView2);
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
  TStoredSessionList * SessionList = GetSessionList(SourceComboBox->ItemIndex);

  SessionListView2->Items->BeginUpdate();
  try
  {
    SessionListView2->Items->Clear();
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

  UnicodeString Error = FErrors->Strings[SourceComboBox->ItemIndex];
  if ((SessionList->Count > 0) || Error.IsEmpty())
  {
    ErrorPanel->Visible = false;
  }
  else
  {
    ErrorLabel->Caption = Error;
    ErrorPanel->Visible = true;
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

  bool Result = (ShowModal() == DefaultResult(this));

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
