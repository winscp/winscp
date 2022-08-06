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
#include <Setup.h>
#include <PuttyTools.h>
#include <GUITools.h>
//---------------------------------------------------------------------------
WINAPI wWinMain(HINSTANCE, HINSTANCE, LPWSTR, int)
{
  AppLog = new TApplicationLog();
  TProgramParams * Params = TProgramParams::Instance();
  UnicodeString AppLogPath;
  if (Params->FindSwitch(L"applog", AppLogPath))
  {
    AppLog->Enable(AppLogPath);
  }
  AppLog->Log(L"Starting...");

  AddStartupSequence(L"M");
  DllHijackingProtection();
  AddStartupSequence(L"T");
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
    AppLog->AddStartupInfo(); // Needs Configuration
    InitializeWinHelp();
    InitializeSystemSettings();
    AddStartupSequence(L"S");

    try
    {
      try
      {
        ConfigureInterface();
        SetupInitialize();

        Application->Title = AppName;
        AppLog->Log(L"Executing...");
        Result = Execute();
        AppLog->Log(L"Execution done");
      }
      catch (Exception & E)
      {
        // Capture most errors before Usage class is released,
        // so that we can count them
        Configuration->Usage->Inc(L"GlobalFailures");
        // After we get WM_QUIT (posted by Application->Terminate()), i.e once Application->Run() exits,
        // the message just blinks
        ShowExtendedException(&E);
      }
    }
    __finally
    {
      AppLog->Log(L"Finalizing");
      GUIFinalize();
      FinalizeSystemSettings();
      FinalizeWinHelp();
      CoreFinalize();
      WinFinalize();
      AppLog->Log(L"Finalizing done");
      SAFE_DESTROY_EX(TApplicationLog, AppLog);
    }
  }
  catch (Exception &E)
  {
    ShowExtendedException(&E);
  }
  return Result;
}
//---------------------------------------------------------------------------
