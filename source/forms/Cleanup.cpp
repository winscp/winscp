//---------------------------------------------------------------------
#include <FormsPCH.h>
#pragma hdrstop

#include <PuttyTools.h>
#include "Cleanup.h"
//---------------------------------------------------------------------
#pragma resource "*.dfm"
//---------------------------------------------------------------------
bool __fastcall DoCleanupDialog()
{
  std::unique_ptr<TCleanupDialog> CleanupDialog(SafeFormCreate<TCleanupDialog>());
  return CleanupDialog->Execute();
}
//---------------------------------------------------------------------
void __fastcall DoCleanupDialogIfAnyDataAndWanted()
{
  std::unique_ptr<TCleanupDialog> CleanupDialog(SafeFormCreate<TCleanupDialog>());
  if (CleanupDialog->AnyData() &&
      (MessageDialog(LoadStr(UNINSTALL_CLEANUP), qtConfirmation, qaYes | qaNo, HELP_UNINSTALL_CLEANUP) == qaYes))
  {
    CleanupDialog->Execute();
  }
}
//---------------------------------------------------------------------
__fastcall TCleanupDialog::TCleanupDialog(TComponent* AOwner)
  : TForm(AOwner)
{
  FAnyData = false;
  FindData();
  UseSystemSettings(this);
}
//---------------------------------------------------------------------
void __fastcall TCleanupDialog::AddLocation(int CaptionId, const UnicodeString & Location, TCleanupEvent Event)
{
  FCaptions.push_back(LoadStr(CaptionId));
  FLocations.push_back(Location);
  FCleanupEvents.push_back(Event);
  FAnyData = true;
}
//---------------------------------------------------------------------
void __fastcall TCleanupDialog::AddRegistryLocation(int CaptionId, const UnicodeString & Location, TCleanupEvent Event)
{
  AddLocation(CaptionId, Configuration->RootKeyStr + L'\\' + Configuration->RegistryStorageKey + L'\\' + Location, Event);
}
//---------------------------------------------------------------------
bool __fastcall TCleanupDialog::AnyData()
{
  return FAnyData;
}
//---------------------------------------------------------------------
void __fastcall TCleanupDialog::FindData()
{
  // Add unconditionally (as it has a side effect of not saving the configuration)
  AddRegistryLocation(CLEANUP_CONFIG, Configuration->ConfigurationSubKey, Configuration->CleanupConfiguration);
  // But count as real data, only if it really exists
  FAnyData = Configuration->RegistryPathExists(Configuration->ConfigurationSubKey);

  if (Configuration->RegistryPathExists(Configuration->StoredSessionsSubKey))
  {
    AddRegistryLocation(CLEANUP_SESSIONS, Configuration->StoredSessionsSubKey, StoredSessions->Cleanup);
  }

  if (Configuration->HasAnyCache())
  {
    AddRegistryLocation(CLEANUP_CACHES, L"...", Configuration->CleanupCaches);
  }

  UnicodeString IniFilePath = ExpandEnvironmentVariables(Configuration->IniFileStorageNameForReading);
  if (FileExists(IniFilePath))
  {
    AddLocation(CLEANUP_INIFILE, IniFilePath, Configuration->CleanupIniFile);
  }

  if (RandomSeedExists())
  {
    AddLocation(CLEANUP_SEEDFILE, Configuration->RandomSeedFileName, Configuration->CleanupRandomSeedFile);
  }

  if (WinConfiguration->AnyTemporaryFolders())
  {
    AddLocation(CLEANUP_TEMP_FOLDERS, WinConfiguration->TemporaryDir(true), WinConfiguration->CleanupTemporaryFolders);
  }
}
//---------------------------------------------------------------------
void __fastcall TCleanupDialog::InitControls()
{
  DebugAssert(FCaptions.size() == FLocations.size());
  DebugAssert(FCaptions.size() == FCleanupEvents.size());
  // Particularly in response to WM_DPICHANGED, the form may re-show
  DataListView->Items->Clear();
  for (size_t Index = 0; Index < FCaptions.size(); Index++)
  {
    TListItem * Item = DataListView->Items->Add();
    Item->Caption = FCaptions[Index];
    Item->SubItems->Add(FLocations[Index]);
  }

  AutoSizeListColumnsWidth(DataListView);
}
//---------------------------------------------------------------------
void __fastcall TCleanupDialog::UpdateControls()
{
  EnableControl(OKButton, ListViewAnyChecked(DataListView));
}
//---------------------------------------------------------------------------
void __fastcall TCleanupDialog::DataListViewMouseDown(
      TObject * /*Sender*/, TMouseButton /*Button*/, TShiftState /*Shift*/,
      int /*X*/, int /*Y*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TCleanupDialog::DataListViewKeyUp(
      TObject * /*Sender*/, WORD & /*Key*/, TShiftState /*Shift*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TCleanupDialog::FormShow(TObject * /*Sender*/)
{
  InitControls();
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TCleanupDialog::CheckAllButtonClick(TObject * /*Sender*/)
{
  ListViewCheckAll(DataListView, caToggle);
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TCleanupDialog::DataListViewInfoTip(TObject * /*Sender*/,
      TListItem * Item, UnicodeString & InfoTip)
{
  InfoTip = Format(L"%s\nLocation: %s",
    ARRAYOFCONST((Item->Caption, Item->SubItems->Strings[0])));
}
//---------------------------------------------------------------------------
void __fastcall TCleanupDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
bool __fastcall TCleanupDialog::Execute()
{
  bool Result = (ShowModal() == DefaultResult(this));
  if (Result)
  {
    Configuration->Usage->Inc(L"Cleanups");

    for (int Index = 0; Index < DataListView->Items->Count; Index++)
    {
      if (DataListView->Items->Item[Index]->Checked)
      {
        try
        {
          FCleanupEvents[Index]();
        }
        catch (Exception & E)
        {
          ShowExtendedException(&E);
        }
      }
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
