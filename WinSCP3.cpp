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
  try
  {
    TProgramParams Params;
    Application->Initialize();
    Initialize(Params.SwitchValue("ini"));
    ConfigureInterface();

    try
    {
      Application->Title = AppName;
      Execute(&Params);
    }
    __finally
    {
      Finalize();
    }
  }
  catch (Exception &E)
  {
    ShowExtendedException(&E);
  }
  return 0;
}
//---------------------------------------------------------------------------
