//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USEFORM("forms\CustomScpExplorer.cpp", CustomScpExplorerForm);
USEFORM("forms\NonVisual.cpp", NonVisualDataModule); /* TDataModule: File Type */
USEFORM("forms\ScpCommander.cpp", ScpCommanderForm);
USEFORM("forms\ScpExplorer.cpp", ScpExplorerForm);
//---------------------------------------------------------------------------
#include <CoreMain.h>
#include <WinInterface.h>
#include <ProgParams.h>
#include <VCLCommon.h>
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  int Result = 0;
  try
  {
    Application->Initialize();
    SetEnvironmentVariable("WINSCP_PATH",
      ExcludeTrailingBackslash(ExtractFilePath(Application->ExeName)).c_str());
    CoreInitialize();
    InitializeWinHelp();
    InitializeSystemSettings();

    try
    {
      ConfigureInterface();

      Application->Title = AppName;
      Result = Execute();
    }
    __finally
    {
      FinalizeSystemSettings();
      FinalizeWinHelp();
      CoreFinalize();
    }
  }
  catch (Exception &E)
  {
    ShowExtendedException(&E);
  }
  return Result;
}
//---------------------------------------------------------------------------
