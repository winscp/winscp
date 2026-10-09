//---------------------------------------------------------------------------
#include <WinPCH.h>
#pragma hdrstop

USEFORM("forms\CustomScpExplorer.cpp", CustomScpExplorerForm);
USEFORM("forms\NonVisual.cpp", NonVisualDataModule); /* TDataModule: File Type */
USEFORM("forms\ScpCommander.cpp", ScpCommanderForm);
USEFORM("forms\ScpExplorer.cpp", ScpExplorerForm);
//---------------------------------------------------------------------------
#include <ProgParams.h>
#include <PuttyTools.h>
#include <signal.h>
//---------------------------------------------------------------------------
void __fastcall AppLogImpl(UnicodeString S)
{
  AppLog(S);
}
//---------------------------------------------------------------------------
NORETURN void TerminateHandler()
{
  AppLog(L"Terminate handler");
  abort();
}
//---------------------------------------------------------------------------
void AbortSignalHandler(int Signal)
{
  AppLogFmt(L"Abort signal handler [%d]", (Signal));
  UnicodeString Message = MainInstructions(L"Abnormal program termination");
  std::unique_ptr<TStrings> MoreMessages(new TStringList());
  UnicodeString ReportErrorText = Trim(FMTLOAD(REPORT_ERROR, (EmptyStr)));
  MoreMessages->Add(ReportErrorText);
  std::unique_ptr<TStrings> StackTrace(GetCurrentStackTrace());
  AppendStackTrace(MoreMessages.get(), StackTrace.get());
  MoreMessageDialog(Message, MoreMessages.get(), qtError, qaOK | qaReport, EmptyStr);
}
//---------------------------------------------------------------------------
int WINAPI wWinMain(HINSTANCE, HINSTANCE, wchar_t *, int)
{
  int Result = 0;
  try
  {
    TProgramParams * Params = TProgramParams::Instance();
    TObjectReleaser<TApplicationLog> ApplicationLogReleaser(ApplicationLog, new TApplicationLog());
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
    AppLogFmt(L"Windows version: %s", (WindowsVersion()));
    AppLogFmt(L"Win32 platform: %d", (Win32Platform()));
    AppLogFmt(L"Windows product type: %x", (static_cast<int>(GetWindowsProductType())));
    AppLogFmt(L"Win64: %s", (BooleanToEngStr(IsWin64())));
    AddStartupSequence(L"T");
    LogModules();

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

    std::set_terminate(TerminateHandler);
    signal(SIGABRT, AbortSignalHandler);

    try
    {
      try
      {
        ConfigureInterface();

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
      AppLogImpl(L"Finalizing"); // AppLog causes internal compiler error
      GUIFinalize();
      FinalizeSystemSettings();
      FinalizeWinHelp();
      CoreFinalize();
      WinFinalize();
      LogModules();
      AppLogImpl(L"Finalizing done");
      OnAppLog = NULL;
    }
  }
  catch (Exception &E)
  {
    ShowExtendedException(&E);
  }
  catch (std::exception &E)
  {
    UnicodeString Message = E.what();
    Message = FORMAT(L"%s (std::exception)", (Message));
    Application->MessageBox(Message.c_str(), L"Fatal Error", MB_OK | MB_ICONERROR);
  }
  catch (...)
  {
    UnicodeString Message = L"Unknown exception";
    MoreMessageDialog(Message, nullptr, qtError, qaOK, EmptyStr);
  }
  return Result;
}
//---------------------------------------------------------------------------
