//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "GeneralSettings.h"

#include <Common.h>
#include <Configuration.h>
#include <ScpMain.h>
#include "WinConfiguration.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "XPGroupBox"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
__fastcall TGeneralSettingsFrame::TGeneralSettingsFrame(TComponent* Owner)
        : TFrame(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TGeneralSettingsFrame::CommanderClick(TObject * /*Sender*/)
{
  CommanderInterfaceButton->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TGeneralSettingsFrame::ExplorerClick(TObject * /*Sender*/)
{
  ExplorerInterfaceButton->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TGeneralSettingsFrame::SaveConfiguration()
{
  WinConfiguration->Interface = CommanderInterfaceButton->Checked ? ifCommander : ifExplorer;
}
//---------------------------------------------------------------------------
void __fastcall TGeneralSettingsFrame::LoadConfiguration()
{
  switch (WinConfiguration->Interface) {
    case ifCommander: CommanderInterfaceButton->Checked = True; break;
    case ifExplorer: ExplorerInterfaceButton->Checked = True; break;
    default: assert(false); break;
  }
}
