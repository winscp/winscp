//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USEFORM("forms\CustomScpExplorer.cpp", CustomScpExplorerForm);
USEFORM("forms\NonVisual.cpp", NonVisualDataModule); /* TDataModule: File Type */
//---------------------------------------------------------------------------
#include <ScpMain.h>
#include <WinInterface.h>
#include <UserInterface.h>
#include <ProgParams.h>
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
    TProgramParams Params;
    // let installer know, that some instance of application is running
    CreateMutex(NULL, False, AppName.c_str());
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

