//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

USEFORM("forms\CustomScpExplorer.cpp", CustomScpExplorerForm);
USEFORM("forms\NonVisual.cpp", NonVisualDataModule); /* TDataModule: File Type */
USEFORM("forms\ScpCommander.cpp", ScpCommanderForm);
USEFORM("forms\ScpExplorer.cpp", ScpExplorerForm);
USEFORM("forms\Glyphs.cpp", GlyphsModule); /* TDataModule: File Type */
//---------------------------------------------------------------------------
#include <CoreMain.h>
#include <WinInterface.h>
#include <ProgParams.h>
#include <VCLCommon.h>
#include <Setup.h>
//---------------------------------------------------------------------------
WINAPI wWinMain(HINSTANCE, HINSTANCE, LPWSTR, int)
{
  int Result = 0;
  try
  {
    WinInitialize();
    Application->Initialize();
    Application->MainFormOnTaskBar = true;
    Application->ModalPopupMode = pmAuto;
    SetEnvironmentVariable(L"WINSCP_PATH",
      ExcludeTrailingBackslash(ExtractFilePath(Application->ExeName)).c_str());
    CoreInitialize();
    InitializeWinHelp();
    InitializeSystemSettings();
    // now everything is setup and mainly the configured locale is already loaded,
    // detect scaling type and possibly forbid further runtime changes to locale
    GUIConfiguration->DetectScalingType();

    try
    {
      try
      {
        ConfigureInterface();
        SetupInitialize();

        Application->Title = AppName;
        Result = Execute();
      }
      catch (Exception & E)
      {
        // Capture most errors before Usage class is released,
        // so that we can count them
        Configuration->Usage->Inc(L"GlobalFailures");
        ShowExtendedException(&E);
      }
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
