//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <VCLCommon.h>
#include <WinConfiguration.h>
#include <TextsWin.h>
#include "Cleanup.h"
//---------------------------------------------------------------------
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------
Boolean __fastcall DoCleanupDialog(TStoredSessionList *SessionList,
  TConfiguration *Configuration)
{
  Boolean Result;
  TCleanupDialog *CleanupDialog;
  try {
    CleanupDialog = SafeFormCreate<TCleanupDialog>();

    CleanupDialog->SessionList = SessionList;
    CleanupDialog->Configuration = Configuration;

    Result = (CleanupDialog->ShowModal() == DefaultResult(CleanupDialog));
    if (Result)
    {
      Configuration->Usage->Inc(L"Cleanups");

      for (int i = wdConfiguration; i <= wdTemporaryFolders; i++)
      {
        if (CleanupDialog->CleanupData[(TWinSCPData)i])
        {
          try
          {
            switch (i)
            {
              case wdConfiguration:
                Configuration->CleanupConfiguration();
                break;

              case wdStoredSessions:
                SessionList->Cleanup();
                break;

              case wdCaches:
                Configuration->CleanupCaches();
                break;

              case wdConfigurationIniFile:
                Configuration->CleanupIniFile();
                break;

              case wdRandomSeedFile:
                Configuration->CleanupRandomSeedFile();
                break;

              case wdTemporaryFolders:
                WinConfiguration->CleanupTemporaryFolders();
                break;
            }
          }
          catch(Exception & E)
          {
            ShowExtendedException(&E);
          }
        }
      }
    }
  } __finally {
    delete CleanupDialog;
  }
  return Result;
}
//---------------------------------------------------------------------
__fastcall TCleanupDialog::TCleanupDialog(TComponent* AOwner)
  : TForm(AOwner)
{
  UseSystemSettings(this);
}
//---------------------------------------------------------------------
void __fastcall TCleanupDialog::InitControls()
{
  // Particularly in response to WM_DPICHANGED, the form may re-show
  DataListView->Items->Clear();

  for (int i = wdConfiguration; i <= wdTemporaryFolders; i++)
  {
    UnicodeString Caption;
    UnicodeString Location;

    switch (i)
    {
      case wdConfiguration:
        Caption = LoadStr(CLEANUP_CONFIG);
        Location = Configuration->ConfigurationSubKey;
        break;

      case wdStoredSessions:
        Caption = LoadStr(CLEANUP_SESSIONS);
        Location = Configuration->StoredSessionsSubKey;
        break;

      case wdCaches:
        Caption = LoadStr(CLEANUP_CACHES);
        Location = L"...";
        break;

      case wdConfigurationIniFile:
        Caption = LoadStr(CLEANUP_INIFILE);
        Location = ExpandEnvironmentVariables(Configuration->IniFileStorageNameForReading);
        break;

      case wdRandomSeedFile:
        Caption = LoadStr(CLEANUP_SEEDFILE);
        Location = ExpandEnvironmentVariables(Configuration->RandomSeedFile);
        break;

      case wdTemporaryFolders:
        Caption = LoadStr(CLEANUP_TEMP_FOLDERS);
        Location = WinConfiguration->TemporaryDir(true);
        break;

      default:
        DebugFail();
        break;
    }

    TListItem * Item = DataListView->Items->Add();
    Item->Caption = Caption;
    if (i < wdConfigurationIniFile)
    {
      Location = Configuration->RootKeyStr + L'\\' +
        Configuration->RegistryStorageKey + L'\\' + Location;
    }

    Item->SubItems->Add(Location);
    DebugAssert(Item->Index == i - 1);
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
bool __fastcall TCleanupDialog::GetCleanupData(TWinSCPData Data)
{
  return DataListView->Items->Item[Data - 1]->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TCleanupDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
