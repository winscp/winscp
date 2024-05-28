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
#pragma resource "*.dfm"
//---------------------------------------------------------------------
const int OpensshIndex = 3;
const int KnownHostsIndex = 5;
const int IniIndex = 4;
//---------------------------------------------------------------------
bool __fastcall DoImportSessionsDialog(TList * Imported)
{
  std::unique_ptr<TImportSessionsDialog> ImportSessionsDialog(
    SafeFormCreate<TImportSessionsDialog>(Application));

  std::unique_ptr<TStrings> Errors(new TStringList());
  std::unique_ptr<TList> SessionListsList(new TList());
  UnicodeString Error;

  std::unique_ptr<TStoredSessionList> PuttyImportSessionList(
    GUIConfiguration->SelectPuttySessionsForImport(OriginalPuttyRegistryStorageKey, L"PuTTY", StoredSessions, Error));
  SessionListsList->Add(PuttyImportSessionList.get());
  Errors->Add(Error);

  std::unique_ptr<TStoredSessionList> KittyImportSessionList(
    GUIConfiguration->SelectPuttySessionsForImport(KittyRegistryStorageKey, L"KiTTY", StoredSessions, Error));
  SessionListsList->Add(KittyImportSessionList.get());
  Errors->Add(Error);

  std::unique_ptr<TStoredSessionList> FilezillaImportSessionList(
    Configuration->SelectFilezillaSessionsForImport(StoredSessions, Error));
  SessionListsList->Add(FilezillaImportSessionList.get());
  Errors->Add(Error);

  std::unique_ptr<TStoredSessionList> OpensshImportSessionList(
    Configuration->SelectOpensshSessionsForImport(StoredSessions, Error));
  SessionListsList->Add(OpensshImportSessionList.get());
  Errors->Add(Error);

  std::unique_ptr<TStoredSessionList> IniImportSessionList(ImportSessionsDialog->SelectSessionsForImport(Error));
  DebugAssert(IniIndex == SessionListsList->Count);
  SessionListsList->Add(IniImportSessionList.get());
  Errors->Add(Error);

  std::unique_ptr<TStoredSessionList> KnownHostsImportSessionList(
    Configuration->SelectKnownHostsSessionsForImport(StoredSessions, Error));
  DebugAssert(KnownHostsIndex == SessionListsList->Count);
  SessionListsList->Add(KnownHostsImportSessionList.get());
  Errors->Add(Error);

  DebugAssert(SessionListsList->Count == Errors->Count);

  ImportSessionsDialog->Init(SessionListsList.get(), Errors.get());

  bool Result = ImportSessionsDialog->Execute();

  if (Result)
  {
    // Particularly when importing known_hosts, there is no feedback.
    TInstantOperationVisualizer Visualizer;

    UnicodeString PuttyHostKeysSourceKey = OriginalPuttyRegistryStorageKey;
    TStoredSessionList * AIniImportSessionList =
      static_cast<TStoredSessionList *>(SessionListsList->Items[IniIndex]);
    TStoredSessionList * AKnownHostsImportSessionList =
      static_cast<TStoredSessionList *>(SessionListsList->Items[KnownHostsIndex]);

    StoredSessions->Import(PuttyImportSessionList.get(), true, Imported);
    TStoredSessionList::ImportHostKeys(PuttyHostKeysSourceKey, PuttyImportSessionList.get(), true);

    StoredSessions->Import(KittyImportSessionList.get(), true, Imported);
    TStoredSessionList::ImportHostKeys(KittyRegistryStorageKey, KittyImportSessionList.get(), true);

    StoredSessions->Import(FilezillaImportSessionList.get(), true, Imported);
    // FileZilla uses PuTTY's host key store
    TStoredSessionList::ImportHostKeys(PuttyHostKeysSourceKey, FilezillaImportSessionList.get(), true);

    StoredSessions->Import(OpensshImportSessionList.get(), true, Imported);

    if (StoredSessions->Import(AIniImportSessionList, true, Imported))
    {
      std::unique_ptr<THierarchicalStorage> IniStorage(TIniFileStorage::CreateFromPath(ImportSessionsDialog->IniFileName));
      TStoredSessionList::ImportHostKeys(IniStorage.get(), AIniImportSessionList, true);
    }

    // The actual import will be done by ImportSelectedKnownHosts
    TStoredSessionList::SelectKnownHostsForSelectedSessions(AKnownHostsImportSessionList, OpensshImportSessionList.get());
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
  BrowseButton->Visible = (SourceComboBox->ItemIndex == IniIndex);
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
TSessionData * TImportSessionsDialog::GetSessionData(TListItem * Item)
{
  return DebugNotNull(static_cast<TSessionData *>(Item->Data));
}
//---------------------------------------------------------------------
void __fastcall TImportSessionsDialog::SaveSelection()
{
  for (int Index = 0; Index < SessionListView2->Items->Count; Index++)
  {
    TListItem * Item = SessionListView2->Items->Item[Index];
    GetSessionData(Item)->Selected = Item->Checked;
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
    SessionListView2->TabStop = true;
  }
  else
  {
    ErrorLabel->Caption = Error;
    ErrorPanel->Visible = true;
    SessionListView2->TabStop = false;
  }

  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TImportSessionsDialog::SessionListView2InfoTip(
      TObject * /*Sender*/, TListItem * Item, UnicodeString & InfoTip)
{
  TSessionData * Data = GetSessionData(Item);
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
bool TImportSessionsDialog::ConvertKeyFile(
  UnicodeString & KeyFile, TStrings * ConvertedKeyFiles, TStrings * NotConvertedKeyFiles)
{
  bool ConvertedSession = false;
  if (!KeyFile.IsEmpty() &&
      FileExists(ApiPath(KeyFile)))
  {
    UnicodeString CanonicalPath = GetCanonicalPath(KeyFile);
    // Reuses the already converted keys saved under a custom name
    // (when saved under the default name they would be captured by the later condition based on GetConvertedKeyFileName)
    int CanonicalIndex = ConvertedKeyFiles->IndexOfName(CanonicalPath);
    if (CanonicalIndex >= 0)
    {
      KeyFile = ConvertedKeyFiles->ValueFromIndex[CanonicalIndex];
      ConvertedSession = true;
    }
    // Prevents asking about converting the same key again, when the user refuses the conversion.
    else if (NotConvertedKeyFiles->IndexOf(CanonicalPath) >= 0)
    {
      // noop
    }
    else
    {
      UnicodeString ConvertedFilename = GetConvertedKeyFileName(KeyFile);
      UnicodeString FileName;
      if (FileExists(ApiPath(ConvertedFilename)))
      {
        FileName = ConvertedFilename;
        ConvertedSession = true;
      }
      else
      {
        FileName = KeyFile;
        TDateTime TimestampBefore, TimestampAfter;
        FileAge(FileName, TimestampBefore);
        try
        {
          VerifyAndConvertKey(FileName, true);
          FileAge(FileName, TimestampAfter);
          if ((KeyFile != FileName) ||
              // should never happen as cancelling the saving throws EAbort
              DebugAlwaysTrue(TimestampBefore != TimestampAfter))
          {
            ConvertedSession = true;
          }
        }
        catch (EAbort &)
        {
          NotConvertedKeyFiles->Add(CanonicalPath);
        }
      }

      if (ConvertedSession)
      {
        KeyFile = FileName;
        ConvertedKeyFiles->Values[CanonicalPath] = FileName;
      }
    }
  }
  return ConvertedSession;
}
//---------------------------------------------------------------------------
bool __fastcall TImportSessionsDialog::Execute()
{
  bool Result = (ShowModal() == DefaultResult(this));

  if (Result)
  {
    ClearSelections();
    SaveSelection();

    if (SourceComboBox->ItemIndex == OpensshIndex)
    {
      std::unique_ptr<TStrings> ConvertedSessions(new TStringList());
      std::unique_ptr<TStrings> ConvertedKeyFiles(new TStringList());
      std::unique_ptr<TStrings> NotConvertedKeyFiles(CreateSortedStringList());
      for (int Index = 0; Index < SessionListView2->Items->Count; Index++)
      {
        TListItem * Item = SessionListView2->Items->Item[Index];
        if (Item->Checked)
        {
          TSessionData * Data = GetSessionData(Item);
          UnicodeString SessionKeys;

          UnicodeString PublicKeyFile = Data->PublicKeyFile;
          if (ConvertKeyFile(PublicKeyFile, ConvertedKeyFiles.get(), NotConvertedKeyFiles.get()))
          {
            Data->PublicKeyFile = PublicKeyFile;
            SessionKeys = PublicKeyFile;
          }

          UnicodeString TunnelPublicKeyFile = Data->TunnelPublicKeyFile;
          if (ConvertKeyFile(TunnelPublicKeyFile, ConvertedKeyFiles.get(), NotConvertedKeyFiles.get()))
          {
            Data->TunnelPublicKeyFile = TunnelPublicKeyFile;
            if (SessionKeys != TunnelPublicKeyFile)
            {
              AddToList(SessionKeys, TunnelPublicKeyFile, L", ");
            }
          }

          if (!SessionKeys.IsEmpty())
          {
            ConvertedSessions->Add(FORMAT(L"%s (%s)", (Data->Name, SessionKeys)));
          }
        }
      }

      if (ConvertedSessions->Count > 0)
      {
        UnicodeString Message = MainInstructions(FMTLOAD(IMPORT_CONVERTED_KEYS, (ConvertedKeyFiles->Count, ConvertedSessions->Count)));
        MoreMessageDialog(Message, ConvertedSessions.get(), qtInformation, qaOK, HelpKeyword);
      }
    }
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
TStoredSessionList * TImportSessionsDialog::SelectSessionsForImport(UnicodeString & Error)
{
  return Configuration->SelectSessionsForImport(StoredSessions, FIniFileName, Error);
}
//---------------------------------------------------------------------------
void __fastcall TImportSessionsDialog::BrowseButtonClick(TObject *)
{
  std::unique_ptr<TOpenDialog> OpenDialog(new TOpenDialog(Application));
  OpenDialog->Title = LoadStr(IMPORT_INI_TITLE);
  OpenDialog->Filter = LoadStr(EXPORT_CONF_FILTER);
  OpenDialog->DefaultExt = L"ini";
  OpenDialog->FileName = DefaultStr(FIniFileName, Configuration->GetDefaultIniFileExportPath());

  if (OpenDialog->Execute())
  {
    SessionListView2->Items->Clear();
    int Index = SourceComboBox->ItemIndex;
    FIniFileName = OpenDialog->FileName;
    UnicodeString Error;
    FIniImportSessionList.reset(SelectSessionsForImport(Error));
    FSessionListsList->Items[Index] = FIniImportSessionList.get();
    FErrors->Strings[Index] = Error;
    LoadSessions();
  }
}
//---------------------------------------------------------------------------
