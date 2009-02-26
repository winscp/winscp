//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "GeneralSettings.h"

#include <Common.h>
#include <Configuration.h>
#include <CoreMain.h>
#include "CustomWinConfiguration.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------------
__fastcall TGeneralSettingsFrame::TGeneralSettingsFrame(TComponent* Owner)
        : TFrame(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TGeneralSettingsFrame::CommanderClick(TObject * /*Sender*/)
{
  CommanderInterfaceButton2->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TGeneralSettingsFrame::ExplorerClick(TObject * /*Sender*/)
{
  ExplorerInterfaceButton2->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TGeneralSettingsFrame::SaveConfiguration()
{
  assert(CustomWinConfiguration);
  CustomWinConfiguration->Interface = CommanderInterfaceButton2->Checked ?
    ifCommander : ifExplorer;
}
//---------------------------------------------------------------------------
void __fastcall TGeneralSettingsFrame::LoadConfiguration()
{
  assert(CustomWinConfiguration);
  switch (CustomWinConfiguration->Interface) {
    case ifCommander: CommanderInterfaceButton2->Checked = True; break;
    case ifExplorer: ExplorerInterfaceButton2->Checked = True; break;
    default: assert(false); break;
  }
}
