//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USEFORM("forms\CustomScpExplorer.cpp", CustomScpExplorerForm);
USEFORM("forms\NonVisual.cpp", NonVisualDataModule); /* TDataModule: File Type */
USEFORM("forms\ScpCommander.cpp", ScpCommanderForm);
USEFORM("forms\ScpExplorer.cpp", ScpExplorerForm);
//---------------------------------------------------------------------------
#include <ScpMain.h>
#include <WinInterface.h>
#include <ProgParams.h>
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  int Result = 0;
  try
  {
    TProgramParams Params;
    Application->Initialize();
    Initialize(Params.SwitchValue("ini"));
    InitializeWebHelp();

    try
    {
      ConfigureInterface();

      Application->Title = AppName;
      Result = Execute(&Params);
    }
    __finally
    {
      Finalize();
      FinalizeWebHelp();
    }
  }
  catch (Exception &E)
  {
    ShowExtendedException(&E);
  }
  return Result;
}
//---------------------------------------------------------------------------
