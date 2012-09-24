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
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
TSessionData * GetLoginData(UnicodeString SessionName, TOptions * Options,
  UnicodeString & DownloadFile, bool & Url)
{
  bool DefaultsOnly;
  TSessionData * Data;

  Data = StoredSessions->ParseUrl(SessionName, Options, DefaultsOnly,
    &DownloadFile, &Url);
  assert(Data != NULL);

  if (!Data->CanLogin || DefaultsOnly)
  {
    if (!DoLoginDialog(StoredSessions, Data, loStartup) || !Data->CanLogin)
    {
      delete Data;
      Data = NULL;
    }
  }
  return Data;
}
//---------------------------------------------------------------------------
void __fastcall Upload(TTerminal * Terminal, TStrings * FileList, bool UseDefaults)
{
  UnicodeString TargetDirectory;
  TGUICopyParamType CopyParam = GUIConfiguration->DefaultCopyParam;

  TargetDirectory = UnixIncludeTrailingBackslash(Terminal->CurrentDirectory);

  int Options = coDisableQueue |
    (!Terminal->IsCapable[fcNewerOnlyUpload] ? coDisableNewerOnly : 0);
  int CopyParamAttrs = Terminal->UsableCopyParamAttrs(0).Upload;
  if (UseDefaults ||
      DoCopyDialog(true, false, FileList, TargetDirectory, &CopyParam, Options,
        CopyParamAttrs, NULL))
  {
    int Params = (CopyParam.NewerOnly ? cpNewerOnly : 0);
    Terminal->CopyToRemote(FileList, TargetDirectory, &CopyParam, Params);
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
      ::SpecialFolderLocation(CSIDL_PERSONAL, LocalDirectory);
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
  Configuration->Usage->Set(L"DefaultLocale",
    IntToHex(static_cast<int>(GetDefaultLCID()), 4));
  Configuration->Usage->Set(L"Locale",
    IntToHex(static_cast<int>(WinConfiguration->Locale), 4));

  UnicodeString ProgramsFolder;
  ::SpecialFolderLocation(CSIDL_PROGRAM_FILES, ProgramsFolder);
  ProgramsFolder = IncludeTrailingBackslash(ExpandFileName(ProgramsFolder));
  UnicodeString ExeName = ExpandFileName(Application->ExeName);
  bool InProgramFiles = AnsiSameText(ExeName.SubString(1, ProgramsFolder.Length()), ProgramsFolder);
  Configuration->Usage->Set(L"InProgramFiles", InProgramFiles);

  StoredSessions->UpdateStaticUsage();
  WinConfiguration->UpdateStaticUsage();

}
//---------------------------------------------------------------------------
void __fastcall MaintenanceTask()
{
  CoreMaintenanceTask();
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

    UnicodeString Value;

    UnicodeString IniFileName = Params->SwitchValue(L"ini");
    if (!IniFileName.IsEmpty())
    {
      UnicodeString IniFileNameExpanded = ExpandEnvironmentVariables(IniFileName);
      if (!FileExists(IniFileNameExpanded))
      {
        // this should be displayed rather at the very beginning.
        // however for simplicity (GUI-only), we do it only here.
        MessageDialog(FMTLOAD(FILE_NOT_EXISTS, (IniFileNameExpanded)), qtError, qaOK);
      }
    }

    if (Params->FindSwitch(L"UninstallCleanup"))
    {
      MaintenanceTask();
      // The innosetup cannot skip UninstallCleanup run task for silent uninstalls,
      // workaround is that we create mutex in uninstaller, if it runs silent, and
      // ignore the UninstallCleanup, when the mutex exists.
      if ((OpenMutex(SYNCHRONIZE, false, L"WinSCPSilentUninstall") == NULL) &&
          (MessageDialog(LoadStr(UNINSTALL_CLEANUP), qtConfirmation,
            qaYes | qaNo, HELP_UNINSTALL_CLEANUP) == qaYes))
      {
        DoCleanupDialog(StoredSessions, Configuration);
      }
    }
    else if (Params->FindSwitch(L"RegisterAsUrlHandler"))
    {
      MaintenanceTask();
      RegisterAsUrlHandler();
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
    else if (Params->FindSwitch(L"Update"))
    {
      MaintenanceTask();
      CheckForUpdates(false);
    }
    else
    {
      TSessionData * Data;
      enum { pcNone, pcUpload, pcFullSynchronize, pcSynchronize } ParamCommand;
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

        if (Params->FindSwitch(L"Upload", CommandParams))
        {
          ParamCommand = pcUpload;
          if (CommandParams->Count == 0)
          {
            throw Exception(NO_UPLOAD_LIST_ERROR);
          }
        }
        if (Params->FindSwitch(L"UploadIfAny", CommandParams))
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
      }

      if (Params->ParamCount > 0)
      {
        AutoStartSession = Params->Param[1];
        Params->ParamsProcessed(1, 1);
        Configuration->Usage->Inc(L"CommandLineSession");
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
        Data = GetLoginData(AutoStartSession, Params, DownloadFile, Url);
        if (Data)
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
            TerminalManager->NewTerminal(Data);
          }
          __finally
          {
            delete Data;
          }

          try
          {
            if (!TerminalManager->ConnectActiveTerminal())
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
              TCustomScpExplorerForm * ScpExplorer = CreateScpExplorer();
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
