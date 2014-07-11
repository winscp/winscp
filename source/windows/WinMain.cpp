//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <CoreMain.h>

#include <Log.h>
#include <TextsWin.h>
#include <TextsCore.h>
#include <HelpWin.h>

#include "CustomScpExplorer.h"
#include "TerminalManager.h"
#include "NonVisual.h"
#include "Glyphs.h"
#include "ProgParams.h"
#include "Setup.h"
#include "WinConfiguration.h"
#include "GUITools.h"
#include "Tools.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
void __fastcall GetLoginData(UnicodeString SessionName, TOptions * Options,
  TObjectList * DataList, UnicodeString & DownloadFile, bool & Url)
{
  bool DefaultsOnly = false;
  bool Close = false;

  if (StoredSessions->IsFolder(SessionName) ||
      StoredSessions->IsWorkspace(SessionName))
  {
    StoredSessions->GetFolderOrWorkspace(SessionName, DataList);
  }
  else
  {
    TSessionData * SessionData =
      StoredSessions->ParseUrl(SessionName, Options, DefaultsOnly,
        &DownloadFile, &Url);
    DataList->Add(SessionData);

    if (DataList->Count == 1)
    {
      TSessionData * SessionData = NOT_NULL(dynamic_cast<TSessionData *>(DataList->Items[0]));
      if (SessionData->SaveOnly)
      {
        Configuration->Usage->Inc(L"CommandLineSessionSave");
        TSessionData * SavedSession = DoSaveSession(SessionData, NULL, true, NULL);
        Close = (SavedSession == NULL);
        if (!Close)
        {
          WinConfiguration->LastStoredSession = SavedSession->Name;
        }
        DataList->Clear();
      }
    }
  }

  if (!Close)
  {
    if ((DataList->Count == 0) ||
        !dynamic_cast<TSessionData *>(DataList->Items[0])->CanLogin ||
        DefaultsOnly)
    {
      // Note that GetFolderOrWorkspace never returns sites that !CanLogin,
      // so we should not get here with more then one site.
      // Though we should be good, if we ever do.
      assert(DataList->Count <= 1);
      if (!DoLoginDialog(StoredSessions, DataList, loStartup))
      {
        DataList->Clear();
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall Upload(TTerminal * Terminal, TStrings * FileList, bool UseDefaults)
{
  UnicodeString TargetDirectory;
  TGUICopyParamType CopyParam = GUIConfiguration->DefaultCopyParam;

  TargetDirectory = UnixIncludeTrailingBackslash(Terminal->CurrentDirectory);

  int Options = coDisableQueue;
  int CopyParamAttrs = Terminal->UsableCopyParamAttrs(0).Upload;
  if (UseDefaults ||
      DoCopyDialog(true, false, FileList, TargetDirectory, &CopyParam, Options,
        CopyParamAttrs, NULL))
  {
    Terminal->CopyToRemote(FileList, TargetDirectory, &CopyParam, 0);
  }
}
//---------------------------------------------------------------------------
void __fastcall Download(TTerminal * Terminal, const UnicodeString FileName,
  bool UseDefaults)
{
  UnicodeString TargetDirectory;
  TGUICopyParamType CopyParam = GUIConfiguration->DefaultCopyParam;
  TStrings * FileList = NULL;

  try
  {
    FileList = new TStringList();
    TRemoteFile * File = Terminal->Files->FindFile(FileName);
    if (File == NULL)
    {
      throw Exception(FMTLOAD(FILE_NOT_EXISTS, (FileName)));
    }
    FileList->AddObject(FileName, File);
    UnicodeString LocalDirectory = ExpandFileName(Terminal->SessionData->LocalDirectory);
    if (LocalDirectory.IsEmpty())
    {
      LocalDirectory = GetPersonalFolder();
    }
    TargetDirectory = IncludeTrailingBackslash(LocalDirectory);

    int Options = coDisableQueue;
    int CopyParamAttrs = Terminal->UsableCopyParamAttrs(0).Download;
    if (UseDefaults ||
        DoCopyDialog(false, false, FileList, TargetDirectory, &CopyParam,
          Options, CopyParamAttrs, NULL))
    {
      Terminal->CopyToLocal(FileList, TargetDirectory, &CopyParam, 0);
    }
  }
  __finally
  {
    delete FileList;
  }
}
//---------------------------------------------------------------------------
void __fastcall Edit(TCustomScpExplorerForm * ScpExplorer, TStrings * FileList)
{
  ScpExplorer->StandaloneEdit(FileList->Strings[0]);
  Abort();
}
//---------------------------------------------------------------------------
void __fastcall SynchronizeDirectories(TTerminal * Terminal,
  TStrings * CommandParams,
  UnicodeString & LocalDirectory, UnicodeString & RemoteDirectory)
{
  if (CommandParams->Count >= 1)
  {
    LocalDirectory = CommandParams->Strings[0];
  }
  else if (!Terminal->SessionData->LocalDirectory.IsEmpty())
  {
    LocalDirectory = ExpandFileName(Terminal->SessionData->LocalDirectory);
  }
  else
  {
    LocalDirectory = WinConfiguration->ScpExplorer.LastLocalTargetDirectory;
  }

  if (CommandParams->Count >= 2)
  {
    RemoteDirectory = CommandParams->Strings[1];
  }
  else
  {
    RemoteDirectory = Terminal->CurrentDirectory;
  }
}
//---------------------------------------------------------------------------
void __fastcall FullSynchronize(TTerminal * Terminal, TCustomScpExplorerForm * ScpExplorer,
  TStrings * CommandParams, bool UseDefaults)
{
  UnicodeString LocalDirectory;
  UnicodeString RemoteDirectory;

  SynchronizeDirectories(Terminal, CommandParams, LocalDirectory, RemoteDirectory);

  bool SaveMode = true;
  // bit ugly
  TSynchronizeMode Mode = (TSynchronizeMode)GUIConfiguration->SynchronizeMode;
  if (ScpExplorer->DoFullSynchronizeDirectories(LocalDirectory,
        RemoteDirectory, Mode, SaveMode, UseDefaults))
  {
    if (SaveMode)
    {
      GUIConfiguration->SynchronizeMode = Mode;
    }

    Terminal->CloseOnCompletion();
  }
  else
  {
    Abort();
  }
}
//---------------------------------------------------------------------------
void __fastcall Synchronize(TTerminal * Terminal, TCustomScpExplorerForm * ScpExplorer,
  TStrings * CommandParams, bool UseDefaults)
{
  UnicodeString LocalDirectory;
  UnicodeString RemoteDirectory;

  SynchronizeDirectories(Terminal, CommandParams, LocalDirectory, RemoteDirectory);

  ScpExplorer->DoSynchronizeDirectories(LocalDirectory, RemoteDirectory, UseDefaults);
  Abort();
}
//---------------------------------------------------------------------------
void __fastcall ImportSitesIfAny()
{
  if (!WinConfiguration->AutoImportedFromPuttyOrFilezilla)
  {
    bool AnyPuttySession = GUIConfiguration->AnyPuttySessionForImport(StoredSessions);
    bool AnyFilezillaSession = GUIConfiguration->AnyFilezillaSessionForImport(StoredSessions);

    if (AnyPuttySession || AnyFilezillaSession)
    {
      UnicodeString PuttySource = LoadStrPart(IMPORT_SESSIONS2, 2);
      UnicodeString FilezillaSource = LoadStrPart(IMPORT_SESSIONS2, 3);
      UnicodeString Source;
      if (AnyPuttySession && AnyFilezillaSession)
      {
        Source = FORMAT(LoadStrPart(IMPORT_SESSIONS2, 4), (PuttySource, FilezillaSource));
      }
      else if (AnyPuttySession)
      {
        Source = PuttySource;
      }
      else if (AnyFilezillaSession)
      {
        Source = FilezillaSource;
      }
      else
      {
        FAIL;
      }

      UnicodeString Message = FORMAT(LoadStrPart(IMPORT_SESSIONS2, 1), (Source));

      if (MessageDialog(Message, qtConfirmation,
            qaOK | qaCancel, HELP_IMPORT_SESSIONS) == qaOK)
      {
        DoImportSessionsDialog(NULL);
      }

      WinConfiguration->AutoImportedFromPuttyOrFilezilla = true;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall Usage(UnicodeString Param)
{
  UnicodeString Key = ::CutToChar(Param, L':', true);
  Configuration->Usage->Set(Key, Param);
}
//---------------------------------------------------------------------------
void __fastcall RecordWrapperVersions(UnicodeString ConsoleVersion, UnicodeString DotNetVersion)
{
  TUpdatesConfiguration Updates = WinConfiguration->Updates;
  if (!DotNetVersion.IsEmpty())
  {
    Updates.DotNetVersion = DotNetVersion;
  }
  if (!ConsoleVersion.IsEmpty())
  {
    Updates.ConsoleVersion = ConsoleVersion;
  }
  WinConfiguration->Updates = Updates;

  if (Configuration->Storage == stNul)
  {
    Configuration->SetDefaultStorage();
    try
    {
      THierarchicalStorage * Storage = Configuration->CreateScpStorage(false);
      try
      {
        Storage->AccessMode = smReadWrite;
        if (Storage->OpenSubKey(Configuration->ConfigurationSubKey, true) &&
            Storage->OpenSubKey(L"Interface\\Updates", true, true))
        {
          if (!DotNetVersion.IsEmpty())
          {
            Storage->WriteString(L"DotNetVersion", DotNetVersion);
          }
          if (!ConsoleVersion.IsEmpty())
          {
            Storage->WriteString(L"ConsoleVersion", ConsoleVersion);
          }
        }
      }
      __finally
      {
        delete Storage;
      }
    }
    __finally
    {
      Configuration->SetNulStorage();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall UpdateStaticUsage()
{
  Configuration->Usage->Inc(L"Runs");

  Configuration->Usage->UpdateCurrentVersion();

  UnicodeString WindowsVersion = FORMAT("%d.%d.%d %s", (Win32MajorVersion, Win32MinorVersion, Win32BuildNumber, Win32CSDVersion));
  Configuration->Usage->Set(L"WindowsVersion", (WindowsVersion));
  Configuration->Usage->Set(L"WindowsProductName", (WindowsProductName()));
  DWORD Type;
  GetWindowsProductType(Type);
  Configuration->Usage->Set(L"WindowsProductType", (static_cast<int>(Type)));
  Configuration->Usage->Set(L"Windows64", IsWin64());
  Configuration->Usage->Set(L"DefaultLocale",
    IntToHex(static_cast<int>(GetDefaultLCID()), 4));
  Configuration->Usage->Set(L"Locale",
    IntToHex(static_cast<int>(WinConfiguration->Locale), 4));
  Configuration->Usage->Set(L"PixelsPerInch", Screen->PixelsPerInch);
  Configuration->Usage->Set(L"WorkAreaWidth", Screen->WorkAreaWidth);
  Configuration->Usage->Set(L"WorkAreaHeight", Screen->WorkAreaHeight);
  Configuration->Usage->Set(L"MonitorCount", Screen->MonitorCount);
  Configuration->Usage->Set(L"NotUseThemes", !UseThemes());
  Configuration->Usage->Set(L"ThemeDefaultFontSize", Application->DefaultFont->Size);
  Configuration->Usage->Set(L"ThemeIconFontSize", Screen->IconFont->Size);

  UnicodeString ProgramsFolder;
  ::SpecialFolderLocation(CSIDL_PROGRAM_FILES, ProgramsFolder);
  ProgramsFolder = IncludeTrailingBackslash(ExpandFileName(ProgramsFolder));
  UnicodeString ExeName = ExpandFileName(Application->ExeName);
  bool InProgramFiles = AnsiSameText(ExeName.SubString(1, ProgramsFolder.Length()), ProgramsFolder);
  Configuration->Usage->Set(L"InProgramFiles", InProgramFiles);
  Configuration->Usage->Set(L"IsInstalled", IsInstalled());
  Configuration->Usage->Set(L"Wine", IsWine());

  WinConfiguration->UpdateStaticUsage();

}
//---------------------------------------------------------------------------
void __fastcall MaintenanceTask()
{
  CoreMaintenanceTask();
}
//---------------------------------------------------------------------------
struct TFindProcessMainWindowParam
{
  unsigned long ProcessId;
  HWND HiddenWindow;
  HWND MainWindow;
};
//---------------------------------------------------------------------------
BOOL __stdcall FindProcessMainWindow(HWND Handle, LPARAM AParam)
{
  TFindProcessMainWindowParam & Param = *reinterpret_cast<TFindProcessMainWindowParam *>(AParam);

  unsigned long ProcessId;
  if ((Handle != Param.HiddenWindow) &&
      (Param.MainWindow == 0) && // optimization
      (GetWindowThreadProcessId(Handle, &ProcessId) != 0) &&
      (ProcessId == Param.ProcessId))
  {
    TCopyDataMessage Message;
    Message.Version = TCopyDataMessage::Version1;

    COPYDATASTRUCT CopyData;
    CopyData.cbData = sizeof(Message);
    CopyData.lpData = &Message;

    Message.Command = TCopyDataMessage::CommandCanCommandLine;

    LRESULT SendResult =
      SendMessage(Handle, WM_COPYDATA, reinterpret_cast<WPARAM>(HInstance),
        reinterpret_cast<LPARAM>(&CopyData));

    if (SendResult > 0)
    {
      Param.MainWindow = Handle;
    }
  }

  return TRUE;
}
//---------------------------------------------------------------------------
bool __fastcall SendToAnotherInstance()
{
  HWND HiddenWindow = FindWindow(HIDDEN_WINDOW_NAME, NULL);
  bool Result = (HiddenWindow != NULL);
  if (Result)
  {
    TCopyDataMessage Message;
    Message.Version = TCopyDataMessage::Version1;

    COPYDATASTRUCT CopyData;
    CopyData.cbData = sizeof(Message);
    CopyData.lpData = &Message;

    // this test is actually redundant, it just a kind of optimization to avoid expensive
    // EnumWindows, we can achieve the same by testing FindProcessMainWindowParam.MainWindow,
    // before sending CommandCommandLine
    Message.Command = TCopyDataMessage::CommandCanCommandLine;
    LRESULT SendResult =
      SendMessage(HiddenWindow, WM_COPYDATA, reinterpret_cast<WPARAM>(HInstance),
        reinterpret_cast<LPARAM>(&CopyData));
    Result = (SendResult > 0);

    if (Result)
    {
      TFindProcessMainWindowParam FindProcessMainWindowParam;
      if (GetWindowThreadProcessId(HiddenWindow, &FindProcessMainWindowParam.ProcessId) != 0)
      {
        FindProcessMainWindowParam.HiddenWindow = HiddenWindow;
        FindProcessMainWindowParam.MainWindow = 0;
        if (EnumWindows(FindProcessMainWindow, reinterpret_cast<LPARAM>(&FindProcessMainWindowParam)) &&
            (FindProcessMainWindowParam.MainWindow != 0))
        {
          // Restore window, if minimized
          ShowWindow(FindProcessMainWindowParam.MainWindow, SW_RESTORE);
          // bring it to foreground
          SetForegroundWindow(FindProcessMainWindowParam.MainWindow);
        }
      }

      Message.Command = TCopyDataMessage::CommandCommandLine;
      wcsncpy(Message.CommandLine, CmdLine, LENOF(Message.CommandLine));
      NULL_TERMINATE(Message.CommandLine);


      LRESULT SendResult =
        SendMessage(HiddenWindow, WM_COPYDATA,
          reinterpret_cast<WPARAM>(HInstance), reinterpret_cast<LPARAM>(&CopyData));
      Result = (SendResult > 0);
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall ShowUpdatesIfAvailable()
{
  TUpdatesConfiguration Updates = WinConfiguration->Updates;
  int CurrentCompoundVer = Configuration->CompoundVersion;
  bool Result =
    Updates.ShowOnStartup &&
    Updates.HaveValidResultsForVersion(CurrentCompoundVer) &&
    !Updates.Results.Disabled &&
    ((Updates.Results.Version > CurrentCompoundVer) || !Updates.Results.Message.IsEmpty()) &&
    !Updates.ShownResults;
  if (Result)
  {
    Configuration->Usage->Inc(L"UpdateStartup");
    Result = CheckForUpdates(true);
    if (Result)
    {
      Configuration->Usage->Inc(L"UpdateDownloadOpensStartup");
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall Execute()
{
  assert(StoredSessions);
  TProgramParams * Params = TProgramParams::Instance();
  assert(Params);

  // do not flash message boxes on startup
  SetOnForeground(true);

  // let installer know, that some instance of application is running
  CreateMutex(NULL, False, AppName.c_str());
  bool OnlyInstance = (GetLastError() == 0);

  UpdateStaticUsage();

  UnicodeString KeyFile;
  if (Params->FindSwitch(L"PrivateKey", KeyFile))
  {
    WinConfiguration->DefaultKeyFile = KeyFile;
  }

  UnicodeString ConsoleVersion;
  UnicodeString DotNetVersion;
  Params->FindSwitch(L"Console", ConsoleVersion);
  Params->FindSwitch(L"DotNet", DotNetVersion);
  if (!ConsoleVersion.IsEmpty() || !DotNetVersion.IsEmpty())
  {
    RecordWrapperVersions(ConsoleVersion, DotNetVersion);
  }
  if (!DotNetVersion.IsEmpty())
  {
    Configuration->Usage->Inc(L"ConsoleDotNet");
  }

  bool Help = Params->FindSwitch(L"help") || Params->FindSwitch(L"h") || Params->FindSwitch(L"?");
  if (Help || Params->FindSwitch(L"Console") || Params->FindSwitch(L"script") ||
      Params->FindSwitch(L"command"))
  {
    return Console(Help);
  }

  TTerminalManager * TerminalManager = NULL;
  GlyphsModule = NULL;
  NonVisualDataModule = NULL;
  TStrings * CommandParams = new TStringList;
  try
  {
    TerminalManager = TTerminalManager::Instance();
    HANDLE ResourceModule = GUIConfiguration->ChangeResourceModule(NULL);
    try
    {
      GlyphsModule = new TGlyphsModule(Application);
    }
    __finally
    {
      GUIConfiguration->ChangeResourceModule(ResourceModule);
    }
    NonVisualDataModule = new TNonVisualDataModule(Application);

    LogForm = NULL;

    Application->HintHidePause = 3000;

    UnicodeString IniFileName = Params->SwitchValue(L"ini");
    if (!IniFileName.IsEmpty())
    {
      UnicodeString IniFileNameExpanded = ExpandEnvironmentVariables(IniFileName);
      if (!FileExists(::ApiPath(IniFileNameExpanded)))
      {
        // this should be displayed rather at the very beginning.
        // however for simplicity (GUI-only), we do it only here.
        MessageDialog(FMTLOAD(FILE_NOT_EXISTS, (IniFileNameExpanded)), qtError, qaOK);
      }
    }

    UnicodeString SwitchValue;
    if (Params->FindSwitch(L"UninstallCleanup"))
    {
      MaintenanceTask();
      // The innosetup cannot skip UninstallCleanup run task for silent uninstalls,
      // workaround is that we create mutex in uninstaller, if it runs silent, and
      // ignore the UninstallCleanup, when the mutex exists.
      if ((OpenMutex(SYNCHRONIZE, false, L"WinSCPSilentUninstall") == NULL) &&
          (MessageDialog(MainInstructions(LoadStr(UNINSTALL_CLEANUP)), qtConfirmation,
            qaYes | qaNo, HELP_UNINSTALL_CLEANUP) == qaYes))
      {
        DoCleanupDialog(StoredSessions, Configuration);
      }
    }
    else if (Params->FindSwitch(L"RegisterForDefaultProtocols") ||
             Params->FindSwitch(L"RegisterAsUrlHandler")) // BACKWARD COMPATIBILITY
    {
      MaintenanceTask();
      RegisterForDefaultProtocols();
    }
    else if (Params->FindSwitch(L"UnregisterForProtocols"))
    {
      MaintenanceTask();
      UnregisterForProtocols();
    }
    else if (Params->FindSwitch(L"AddSearchPath"))
    {
      MaintenanceTask();
      AddSearchPath(ExtractFilePath(Application->ExeName));
    }
    else if (Params->FindSwitch(L"RemoveSearchPath"))
    {
      MaintenanceTask();
      try
      {
        RemoveSearchPath(ExtractFilePath(Application->ExeName));
      }
      catch(...)
      {
        // ignore errors
        // (RemoveSearchPath is called always on uninstallation,
        // even if AddSearchPath was not used, so we would get the error
        // always for non-priviledged user)
      }
    }
    else if (Params->FindSwitch(L"ImportSitesIfAny"))
    {
      MaintenanceTask();
      ImportSitesIfAny();
    }
    else if (Params->FindSwitch(L"Usage", SwitchValue))
    {
      MaintenanceTask();
      Usage(SwitchValue);
    }
    else if (Params->FindSwitch(L"Update"))
    {
      MaintenanceTask();
      CheckForUpdates(false);
    }
    else if (ShowUpdatesIfAvailable())
    {
      // noop
    }
    else
    {
      enum { pcNone, pcUpload, pcFullSynchronize, pcSynchronize, pcEdit } ParamCommand;
      ParamCommand = pcNone;
      UnicodeString AutoStartSession;
      UnicodeString DownloadFile;
      bool UseDefaults = false;

      // do not check for temp dirs for service tasks (like RegisterAsUrlHandler)
      if (OnlyInstance &&
          WinConfiguration->TemporaryDirectoryCleanup)
      {
        TemporaryDirectoryCleanup();
      }

      WinConfiguration->CheckDefaultTranslation();

      if (!Params->Empty)
      {
        if (Params->FindSwitch(L"Defaults"))
        {
          UseDefaults = true;
        }

        if (Params->FindSwitch(UPLOAD_SWITCH, CommandParams))
        {
          ParamCommand = pcUpload;
          if (CommandParams->Count == 0)
          {
            throw Exception(NO_UPLOAD_LIST_ERROR);
          }
        }
        if (Params->FindSwitch(UPLOAD_IF_ANY_SWITCH, CommandParams))
        {
          if (CommandParams->Count > 0)
          {
            ParamCommand = pcUpload;
          }
        }
        else if (Params->FindSwitch(L"Synchronize", CommandParams, 2))
        {
          ParamCommand = pcFullSynchronize;
        }
        else if (Params->FindSwitch(L"KeepUpToDate", CommandParams, 2))
        {
          ParamCommand = pcSynchronize;
        }
        else if (Params->FindSwitch(L"Edit", CommandParams, 1) &&
                 (CommandParams->Count == 1))
        {
          ParamCommand = pcEdit;
        }
      }

      if (Params->ParamCount > 0)
      {
        if ((ParamCommand == pcNone) &&
            (WinConfiguration->ExternalSessionInExistingInstance != OpenInNewWindow()) &&
            !Params->FindSwitch(L"NewInstance") &&
            SendToAnotherInstance())
        {
          Configuration->Usage->Inc(L"SendToAnotherInstance");
          return 0;
        }
        AutoStartSession = Params->Param[1];
        Params->ParamsProcessed(1, 1);
        UnicodeString CounterName;
        if (Params->FindSwitch(JUMPLIST_SWITCH))
        {
          CounterName = L"CommandLineJumpList";
        }
        else if (Params->FindSwitch(DESKTOP_SWITCH))
        {
          CounterName = L"CommandLineDesktop";
        }
        else if (Params->FindSwitch(SEND_TO_HOOK_SWITCH))
        {
          CounterName = L"CommandLineSendToHook";
        }
        else
        {
          CounterName = L"CommandLineSession2";
        }
        Configuration->Usage->Inc(CounterName);
      }
      else if (WinConfiguration->EmbeddedSessions && StoredSessions->Count)
      {
        AutoStartSession = StoredSessions->Sessions[0]->Name;
      }
      else
      {
        AutoStartSession = WinConfiguration->AutoStartSession;
      }

      // from now flash message boxes on background
      SetOnForeground(false);

      bool Retry;
      do
      {
        Retry = false;
        bool Url = false;
        TObjectList * DataList = new TObjectList();
        GetLoginData(AutoStartSession, Params, DataList, DownloadFile, Url);
        // from now on, we do not support runtime locale change
        GUIConfiguration->CanApplyLocaleImmediately = false;
        try
        {
          if (DataList->Count > 0)
          {
            if (Url || Params->FindSwitch(L"Unsafe"))
            {
              // prevent any automatic action when URL is provided on
              // command-line (the check is duplicated in Console())
              if (UseDefaults || Params->FindSwitch(L"Log") || Params->FindSwitch(L"XmlLog"))
              {
                MessageDialog(LoadStr(UNSAFE_ACTIONS_DISABLED), qtWarning, qaOK);
              }
              UseDefaults = false;
            }
            else
            {
              UnicodeString LogFile;
              if (Params->FindSwitch(L"Log", LogFile))
              {
                Configuration->TemporaryLogging(LogFile);
              }
              if (Params->FindSwitch(L"XmlLog", LogFile))
              {
                Configuration->TemporaryActionsLogging(LogFile);
              }
            }

            try
            {
              assert(!TerminalManager->ActiveTerminal);

              TerminalManager->ActiveTerminal =
                TerminalManager->NewTerminals(DataList);

              if (TerminalManager->Count == 0)
              {
                // do not prompt with login dialog, if connection of
                // auto-start session (typically from command line) failed
                if (AutoStartSession.IsEmpty())
                {
                  Retry = true;
                }
              }
              else
              {
                // from now on, we do not support runtime interface change
                CustomWinConfiguration->CanApplyInterfaceImmediately = false;
                TCustomScpExplorerForm * ScpExplorer = CreateScpExplorer();
                CustomWinConfiguration->AppliedInterface = CustomWinConfiguration->Interface;
                try
                {
                  // moved inside try .. __finally, because it can fail as well
                  TerminalManager->ScpExplorer = ScpExplorer;

                  if ((ParamCommand != pcNone) || !DownloadFile.IsEmpty())
                  {
                    Configuration->Usage->Inc(L"CommandLineOperation");
                  }

                  if (ParamCommand == pcUpload)
                  {
                    Upload(TerminalManager->ActiveTerminal, CommandParams, UseDefaults);
                  }
                  else if (ParamCommand == pcFullSynchronize)
                  {
                    FullSynchronize(TerminalManager->ActiveTerminal, ScpExplorer,
                      CommandParams, UseDefaults);
                  }
                  else if (ParamCommand == pcSynchronize)
                  {
                    Synchronize(TerminalManager->ActiveTerminal, ScpExplorer,
                      CommandParams, UseDefaults);
                  }
                  else if (ParamCommand == pcEdit)
                  {
                    Edit(ScpExplorer, CommandParams);
                  }
                  else if (!DownloadFile.IsEmpty())
                  {
                    Download(TerminalManager->ActiveTerminal, DownloadFile,
                      UseDefaults);
                  }

                  Application->Run();
                }
                __finally
                {
                  TerminalManager->ScpExplorer = NULL;
                  SAFE_DESTROY(ScpExplorer);
                }
              }
            }
            catch (Exception &E)
            {
              ShowExtendedException(&E);
            }
          }
        }
        __finally
        {
          delete DataList;
        }
      }
      while (Retry);
    }
  }
  __finally
  {
    delete NonVisualDataModule;
    NonVisualDataModule = NULL;
    delete GlyphsModule;
    GlyphsModule = NULL;
    TTerminalManager::DestroyInstance();
    delete CommandParams;
  }

  return 0;
}
