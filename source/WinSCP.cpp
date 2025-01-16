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
#include <Tools.h>
//---------------------------------------------------------------------------
void __fastcall AppLogImpl(UnicodeString S)
{
  AppLog(S);
}
//---------------------------------------------------------------------------
WINAPI wWinMain(HINSTANCE, HINSTANCE, LPWSTR, int)
{
  int Result = 0;
  try
  {
    TProgramParams * Params = TProgramParams::Instance();
    ApplicationLog = new TApplicationLog();
    UnicodeString AppLogPath;
    if (Params->FindSwitch(L"applog", AppLogPath))
    {
      ApplicationLog->Enable(AppLogPath);
      OnAppLog = AppLogImpl;
    }
    AppLog(L"Starting...");
    if (Params->FindSwitch(L"IsUWP"))
    {
      EnableUWPTestMode();
    }

    AddStartupSequence(L"M");
    AppLogFmt(L"Process: %d", (GetCurrentProcessId()));
    AppLogFmt(L"Mouse: %s", (BooleanToEngStr(Mouse->MousePresent)));
    AppLogFmt(L"Mouse wheel: %s, msg: %d, scroll lines: %d", (BooleanToEngStr(Mouse->WheelPresent), int(Mouse->RegWheelMessage), Mouse->WheelScrollLines));
    AppLogFmt(L"ACP: %d", (static_cast<int>(GetACP())));
    AppLogFmt(L"Win32 platform: %d", (Win32Platform()));
    AppLogFmt(L"Windows product type: %x", (static_cast<int>(GetWindowsProductType())));
    AppLogFmt(L"Win64: %s", (BooleanToEngStr(IsWin64())));
    AddStartupSequence(L"T");

    WinInitialize();
    Application->Initialize();
    Application->MainFormOnTaskBar = true;
    Application->ModalPopupMode = pmAuto;
    DebugAssert(SameFont(Application->DefaultFont, std::unique_ptr<TFont>(new TFont()).get()));
    SetEnvironmentVariable(L"WINSCP_PATH",
      ExcludeTrailingBackslash(ExtractFilePath(Application->ExeName)).c_str());
    CoreInitialize();
    ApplicationLog->AddStartupInfo(); // Needs Configuration
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
        AppLog(L"Executing...");
        Result = Execute();
        AppLog(L"Execution done");
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
      AppLog(L"Finalizing");
      GUIFinalize();
      FinalizeSystemSettings();
      FinalizeWinHelp();
      CoreFinalize();
      WinFinalize();
      AppLog(L"Finalizing done");
      OnAppLog = NULL;
      SAFE_DESTROY_EX(TApplicationLog, ApplicationLog);
    }
  }
  catch (Exception &E)
  {
    ShowExtendedException(&E);
  }
  return Result;
}
//---------------------------------------------------------------------------
