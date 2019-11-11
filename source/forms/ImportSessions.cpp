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
#include <Tools.h>
#include <WinApi.h>
#include <PasTools.hpp>
//---------------------------------------------------------------------
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------
const int KnownHostsIndex = 2;
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
  std::unique_ptr<TStoredSessionList> KnownHostsImportSessionList(
    Configuration->SelectKnownHostsSessionsForImport(StoredSessions, Error));
  Errors->Add(Error);

  std::unique_ptr<TList> SessionListsList(new TList());
  SessionListsList->Add(PuttyImportSessionList.get());
  SessionListsList->Add(FilezillaImportSessionList.get());
  SessionListsList->Add(KnownHostsImportSessionList.get());

  std::unique_ptr<TImportSessionsDialog> ImportSessionsDialog(
    SafeFormCreate<TImportSessionsDialog>(Application));

  ImportSessionsDialog->Init(SessionListsList.get(), Errors.get());

  bool Result = ImportSessionsDialog->Execute();

  if (Result)
  {
    // Particularly when importing known_hosts, there is no feedback.
    TInstantOperationVisualizer Visualizer;

    StoredSessions->Import(PuttyImportSessionList.get(), true, Imported);
    StoredSessions->Import(FilezillaImportSessionList.get(), true, Imported);

    UnicodeString SourceKey = Configuration->PuttyRegistryStorageKey;

    TStoredSessionList::ImportHostKeys(SourceKey, PuttyImportSessionList.get(), true);

    // Filezilla uses PuTTY's host key store
    TStoredSessionList::ImportHostKeys(SourceKey, FilezillaImportSessionList.get(), true);

    TStoredSessionList * AKnownHostsImportSessionList =
      static_cast<TStoredSessionList *>(SessionListsList->Items[KnownHostsIndex]);
    TStoredSessionList::ImportSelectedKnownHosts(AKnownHostsImportSessionList);
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
  FSessionListsList = SessionListsList;
  FErrors = Errors;

  for (int Index = 0; Index < SessionListsList->Count; Index++)
  {
    if ((SourceComboBox->ItemIndex < 0) && (GetSessionList(Index)->Count > 0))
    {
      SourceComboBox->ItemIndex = Index;
    }
  }

  if (SourceComboBox->ItemIndex < 0)
  {
    SourceComboBox->ItemIndex = 0;
  }

  int Offset = ScaleByTextHeight(this, 8);
  ErrorPanel->BoundsRect =
    TRect(
      SessionListView2->BoundsRect.Left + Offset, SessionListView2->BoundsRect.Top + Offset,
      SessionListView2->BoundsRect.Right - Offset, SessionListView2->BoundsRect.Bottom - Offset);

}
//---------------------------------------------------------------------
TStoredSessionList * __fastcall TImportSessionsDialog::GetSessionList(int Index)
{
  return reinterpret_cast<TStoredSessionList *>(FSessionListsList->Items[Index]);
}
//---------------------------------------------------------------------
void __fastcall TImportSessionsDialog::UpdateControls()
{
  PasteButton->Visible = (SourceComboBox->ItemIndex == KnownHostsIndex);
  EnableControl(PasteButton, IsFormatInClipboard(CF_TEXT));
  EnableControl(OKButton, ListViewAnyChecked(SessionListView2));
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
  TSessionData * Data = DebugNotNull(reinterpret_cast<TSessionData *>(Item->Data));
  if (SourceComboBox->ItemIndex == KnownHostsIndex)
  {
    UnicodeString Algs;
    UnicodeString HostKeys = Data->HostKey;
    while (!HostKeys.IsEmpty())
    {
      UnicodeString HostKey = CutToChar(HostKeys, L';', true);
      UnicodeString Alg = CutToChar(HostKey, L':', true);
      AddToList(Algs, Alg, L", ");
    }

    InfoTip = FMTLOAD(IMPORT_KNOWNHOSTS_INFO_TIP, (Data->HostName, Algs));
  }
  else
  {
    InfoTip = Data->InfoTip;
  }
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
  // Load only now, as earlier loading somehow breaks SessionListView2 layout on initial per-monitor DPI scaling
  LoadSessions();
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
bool __fastcall TImportSessionsDialog::Execute()
{
  bool Result = (ShowModal() == DefaultResult(this));

  if (Result)
  {
    ClearSelections();
    SaveSelection();
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
void __fastcall TImportSessionsDialog::CreateHandle()
{
  TForm::CreateHandle();

  if (DebugAlwaysTrue(HandleAllocated()))
  {
    HINSTANCE User32Library = LoadLibrary(L"user32.dll");
    AddClipboardFormatListenerProc AddClipboardFormatListener =
      (AddClipboardFormatListenerProc)GetProcAddress(User32Library, "AddClipboardFormatListener");
    if (AddClipboardFormatListener != NULL)
    {
      AddClipboardFormatListener(Handle);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TImportSessionsDialog::DestroyHandle()
{
  if (DebugAlwaysTrue(HandleAllocated()))
  {
    HINSTANCE User32Library = LoadLibrary(L"user32.dll");
    RemoveClipboardFormatListenerProc RemoveClipboardFormatListener =
      (RemoveClipboardFormatListenerProc)GetProcAddress(User32Library, "RemoveClipboardFormatListener");
    if (RemoveClipboardFormatListener != NULL)
    {
      RemoveClipboardFormatListener(Handle);
    }
  }

  TForm::DestroyHandle();
}
//---------------------------------------------------------------------------
void __fastcall TImportSessionsDialog::Dispatch(void * Message)
{
  TMessage * M = static_cast<TMessage*>(Message);
  switch (M->Msg)
  {
    case WM_CLIPBOARDUPDATE:
      UpdateControls();
      TForm::Dispatch(Message);
      break;

    default:
      TForm::Dispatch(Message);
      break;
  }
}
//---------------------------------------------------------------------------
void __fastcall TImportSessionsDialog::PasteButtonClick(TObject * /*Sender*/)
{
  UnicodeString Text;
  // Proceed even when retrieving from clipboard fails, "no host keys" error will show.
  TextFromClipboard(Text, false);
  std::unique_ptr<TStrings> Lines(new TStringList());
  Lines->Text = Text;
  SessionListView2->Items->Clear();
  int Index = SourceComboBox->ItemIndex;
  UnicodeString Error;
  FPastedKnownHosts.reset(Configuration->SelectKnownHostsSessionsForImport(Lines.get(), StoredSessions, Error));
  FSessionListsList->Items[Index] = FPastedKnownHosts.get();
  FErrors->Strings[Index] = Error;
  LoadSessions();
}
//---------------------------------------------------------------------------
