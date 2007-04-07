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
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  int Result = 0;
  try
  {
    Application->Initialize();
    CoreInitialize();
    InitializeWinHelp();

    try
    {
      ConfigureInterface();

      Application->Title = AppName;
      Result = Execute();
    }
    __finally
    {
      CoreFinalize();
      FinalizeWinHelp();
    }
  }
  catch (Exception &E)
  {
    ShowExtendedException(&E);
  }
  return Result;
}
//---------------------------------------------------------------------------
