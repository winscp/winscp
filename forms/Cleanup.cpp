//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <VCLCommon.h>
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
      if (CleanupDialog->CleanupData[wdConfiguration])
        Configuration->CleanupConfiguration();
      if (CleanupDialog->CleanupData[wdStoredSessions])
        SessionList->Cleanup();
      if (CleanupDialog->CleanupData[wdHostKeys])
        Configuration->CleanupHostKeys();
      if (CleanupDialog->CleanupData[wdConfigurationIniFile])
        Configuration->CleanupIniFile();
      if (CleanupDialog->CleanupData[wdRandomSeedFile])
        Configuration->CleanupRandomSeedFile();
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
  UseSystemFont(this);
}
//---------------------------------------------------------------------
void __fastcall TCleanupDialog::UpdateControls()
{
  Boolean Checked = False, UnChecked = False;
  for (Integer Index = 0; Index < DataListView->Items->Count; Index ++)
  {
    TListItem *Item = DataListView->Items->Item[Index];
    if (Item->Checked) Checked = True;
    if (!Item->Checked) UnChecked = True;
    AnsiString Location;
    switch (Item->ImageIndex) {
      case wdConfiguration: Location = Configuration->ConfigurationSubKey; break;
      case wdStoredSessions: Location = Configuration->StoredSessionsSubKey; break;
      case wdHostKeys: Location = Configuration->SshHostKeysSubKey; break;
      case wdConfigurationIniFile: Location = Configuration->IniFileStorageName; break;
      case wdRandomSeedFile: Location = Configuration->RandomSeedFile; break;
      default: Location = ""; break;
    }
    if (Item->ImageIndex < wdConfigurationIniFile)
      Location = Configuration->RootKeyStr + '\\' +
        Configuration->RegistryStorageKey + '\\' + Location;

    if (Item->SubItems->Count)
    {
      if (Item->SubItems->Strings[0] != Location)
        Item->SubItems->Strings[0] = Location;
    }
      else Item->SubItems->Add(Location);
  }
  EnableControl(OKButton, Checked);
  EnableControl(CheckAllButton, UnChecked);
//  AdjustListColumnsWidth(DataListView);
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
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TCleanupDialog::CheckAllButtonClick(TObject * /*Sender*/)
{
  for (Integer Index = 0; Index < DataListView->Items->Count; Index ++)
    DataListView->Items->Item[Index]->Checked = True;
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
