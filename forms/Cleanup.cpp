//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <VCLCommon.h>
#include <WinConfiguration.h>
#include <TextsWin.h>
#include "Cleanup.h"
//---------------------------------------------------------------------
#pragma resource "*.dfm"
//---------------------------------------------------------------------
Boolean __fastcall DoCleanupDialog(TStoredSessionList *SessionList,
  TConfiguration *Configuration)
{
  Boolean Result;
  TCleanupDialog *CleanupDialog;
  try {
    CleanupDialog = new TCleanupDialog(Application);

    CleanupDialog->SessionList = SessionList;
    CleanupDialog->Configuration = Configuration;

    Result = (CleanupDialog->ShowModal() == mrOk);
    if (Result)
    {
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

              case wdHostKeys:
                Configuration->CleanupHostKeys();
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
  static const int Captions[] = {
    CLEANUP_CONFIG, CLEANUP_SESSIONS, CLEANUP_HOSTKEYS, CLEANUP_INIFILE,
    CLEANUP_SEEDFILE, CLEANUP_TEMP_FOLDERS };

  int I = 0;
  while (I < DataListView->Items->Count)
  {
    TListItem *Item = DataListView->Items->Item[I];
    AnsiString Location;
    Item->Caption = LoadStr(Captions[Item->ImageIndex - 1]);
    switch (Item->ImageIndex) {
      case wdConfiguration: Location = Configuration->ConfigurationSubKey; break;
      case wdStoredSessions: Location = Configuration->StoredSessionsSubKey; break;
      case wdHostKeys: Location = Configuration->SshHostKeysSubKey; break;
      case wdConfigurationIniFile: Location = Configuration->IniFileStorageName; break;
      case wdRandomSeedFile: Location = Configuration->RandomSeedFile; break;
      case wdTemporaryFolders: Location = WinConfiguration->TemporaryDir(true); break;
      default: Location = ""; break;
    }

    if (Item->ImageIndex < wdConfigurationIniFile)
    {
      Location = Configuration->RootKeyStr + '\\' +
        Configuration->RegistryStorageKey + '\\' + Location;
    }

    Item->SubItems->Add(Location);
    I++;
  }
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
      TListItem * Item, AnsiString & InfoTip)
{
  InfoTip = Format("%s\nLocation: %s",
    ARRAYOFCONST((Item->Caption, Item->SubItems->Strings[0])));
}
//---------------------------------------------------------------------------
void __fastcall TCleanupDialog::SetCleanupData(TWinSCPData Data, Boolean value)
{
  for (Integer Index = 0; Index < DataListView->Items->Count; Index ++)
  {
    TListItem *Item = DataListView->Items->Item[Index];
    if ((Item->ImageIndex == Data) && (Item->Checked != value))
    {
      Item->Checked = value;
      UpdateControls();
    }
  }
}
//---------------------------------------------------------------------------
Boolean __fastcall TCleanupDialog::GetCleanupData(TWinSCPData Data)
{
  for (Integer Index = 0; Index < DataListView->Items->Count; Index ++)
  {
    TListItem *Item = DataListView->Items->Item[Index];
    if (Item->ImageIndex == Data) return Item->Checked;
  }
  return False;
}
