//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "UserInterface.h"
#include "ScpCommander.h"
#include "ScpExplorer.h"
#include <About.h>

#include <ScpMain.h>
#include "WinConfiguration.h"
#include "TextsWin.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
const AnsiString AppName = "WinSCP";
const AnsiString AppNameVersion = "WinSCP3";
//---------------------------------------------------------------------------
TCustomScpExplorerForm * __fastcall CreateScpExplorer()
{
  TCustomScpExplorerForm * ScpExplorer;
  if (WinConfiguration->Interface == ifExplorer)
    Application->CreateForm(__classid(TScpExplorerForm), &ScpExplorer);
  else
    Application->CreateForm(__classid(TScpCommanderForm), &ScpExplorer);
  ScpExplorer->Icon->Assign(Application->Icon);
  return ScpExplorer;
}
//---------------------------------------------------------------------------
AnsiString __fastcall GetRegistryKey()
{
  return "Software\\Martin Prikryl\\WinSCP 2";
}
//---------------------------------------------------------------------------
void __fastcall ConfigureInterface()
{
  AnsiString S;
  S = LoadStr(MIDDLE_EAST);
  if (!S.IsEmpty())
  {
    SysLocale.MiddleEast = static_cast<bool>(StrToInt(S));
  }
  else
  {
    SysLocale.MiddleEast = false;
  }
  S = LoadStr(BIDI_MODE);
  if (!S.IsEmpty())
  {
    Application->BiDiMode = static_cast<TBiDiMode>(StrToInt(bdRightToLeft));
  }
  else
  {
    Application->BiDiMode = bdLeftToRight;
  }
}
//---------------------------------------------------------------------------
void __fastcall DoAboutDialog(TConfiguration *Configuration)
{
  TAboutDialog *AboutDialog = NULL;
  try
  {
    AboutDialog = new TAboutDialog(Application);
    AboutDialog->Configuration = Configuration;
    AboutDialog->ShowModal();
  }
  __finally
  {
    delete AboutDialog;
  }
}
//---------------------------------------------------------------------
void __fastcall DoProductLicence()
{
  DoLicenceDialog(lcWinScp);
}
//---------------------------------------------------------------------
