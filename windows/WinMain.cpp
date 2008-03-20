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
TSessionData * GetLoginData(AnsiString SessionName, TOptions * Options,
  AnsiString & DownloadFile, bool & Url)
{
  bool DefaultsOnly;
  TSessionData * Data;

  Data = StoredSessions->ParseUrl(SessionName, Options, DefaultsOnly,
    puExtractFileName | puDecodeUrlChars, &DownloadFile, &Url);
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
  AnsiString TargetDirectory;
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
void __fastcall Download(TTerminal * Terminal, const AnsiString FileName,
  bool UseDefaults)
{
  AnsiString TargetDirectory;
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
    AnsiString LocalDirectory = Terminal->SessionData->LocalDirectory;
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
  AnsiString & LocalDirectory, AnsiString & RemoteDirectory)
{
  if (CommandParams->Count >= 1)
  {
    LocalDirectory = CommandParams->Strings[0];
  }
  else if (!Terminal->SessionData->LocalDirectory.IsEmpty())
  {
    LocalDirectory = Terminal->SessionData->LocalDirectory;
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
  AnsiString LocalDirectory;
  AnsiString RemoteDirectory;

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
  AnsiString LocalDirectory;
  AnsiString RemoteDirectory;

  SynchronizeDirectories(Terminal, CommandParams, LocalDirectory, RemoteDirectory);

  ScpExplorer->DoSynchronizeDirectories(LocalDirectory, RemoteDirectory, UseDefaults);
  Abort();
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

  AnsiString KeyFile;
  if (Params->FindSwitch("PrivateKey", KeyFile))
  {
    WinConfiguration->DefaultKeyFile = KeyFile;
  }

  bool Help = Params->FindSwitch("help") || Params->FindSwitch("h") || Params->FindSwitch("?");
  if (Help || Params->FindSwitch("Console") || Params->FindSwitch("script") ||
      Params->FindSwitch("command"))
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
    HINSTANCE ResourceModule = GUIConfiguration->ChangeResourceModule(NULL);
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

    AnsiString Value;

    AnsiString IniFileName = Params->SwitchValue("ini");
    if (!IniFileName.IsEmpty())
    {
      AnsiString IniFileNameExpanded = ExpandEnvironmentVariables(IniFileName);
      if (!FileExists(IniFileNameExpanded))
      {
        // this should be displayed rather at the very beginning.
        // however for simplicity (GUI-only), we do it only here.
        MessageDialog(FMTLOAD(FILE_NOT_EXISTS, (IniFileNameExpanded)), qtError, qaOK);
      }
    }

    if (Params->FindSwitch("UninstallCleanup"))
    {
      // The innosetup cannot skip UninstallCleanup run task for silent uninstalls,
      // workaround is that we create mutex in uninstaller, if it runs silent, and
      // ignore the UninstallCleanup, when the mutex exists.
      if ((OpenMutex(SYNCHRONIZE, false, "WinSCPSilentUninstall") == NULL) &&
          (MessageDialog(LoadStr(UNINSTALL_CLEANUP), qtConfirmation,
            qaYes | qaNo, HELP_UNINSTALL_CLEANUP) == qaYes))
      {
        DoCleanupDialog(StoredSessions, Configuration);
      }
    }
    else if (Params->FindSwitch("RegisterAsUrlHandler"))
    {
      RegisterAsUrlHandler();
    }
    else if (Params->FindSwitch("AddSearchPath"))
    {
      AddSearchPath(ExtractFilePath(Application->ExeName));
    }
    else if (Params->FindSwitch("RemoveSearchPath"))
    {
      RemoveSearchPath(ExtractFilePath(Application->ExeName));
    }
    else if (Params->FindSwitch("Update"))
    {
      CheckForUpdates(false);
    }
    else
    {
      TSessionData * Data;
      enum { pcNone, pcUpload, pcFullSynchronize, pcSynchronize } ParamCommand;
      ParamCommand = pcNone;
      AnsiString AutoStartSession;
      AnsiString DownloadFile;
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
        if (Params->FindSwitch("Defaults"))
        {
          UseDefaults = true;
        }

        if (Params->FindSwitch("Upload", CommandParams))
        {
          ParamCommand = pcUpload;
          if (CommandParams->Count == 0)
          {
            throw Exception(NO_UPLOAD_LIST_ERROR);
          }
        }
        if (Params->FindSwitch("UploadIfAny", CommandParams))
        {
          if (CommandParams->Count > 0)
          {
            ParamCommand = pcUpload;
          }
        }
        else if (Params->FindSwitch("Synchronize", CommandParams, 2))
        {
          ParamCommand = pcFullSynchronize;
        }
        else if (Params->FindSwitch("KeepUpToDate", CommandParams, 2))
        {
          ParamCommand = pcSynchronize;
        }
      }

      if (Params->ParamCount > 0)
      {
        AutoStartSession = Params->Param[1];
        Params->ParamsProcessed(1, 1);
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
          if (Url || Params->FindSwitch("Unsafe"))
          {
            // prevent any automatic action when URL is provided on
            // command-line (the check is duplicated in Console())
            if (UseDefaults || Params->FindSwitch("Log"))
            {
              MessageDialog(LoadStr(UNSAFE_ACTIONS_DISABLED), qtWarning, qaOK);
            }
            UseDefaults = false;
          }
          else
          {
            AnsiString LogFile;
            if (Params->FindSwitch("Log", LogFile))
            {
              Configuration->TemporaryLogging(LogFile);
            }
          }

          if ((Data->FSProtocol == fsExternalSSH) ||
              (Data->FSProtocol == fsExternalSFTP))
          {
            OpenSessionInPutty(
              ((Data->FSProtocol == fsExternalSSH) ?
                GUIConfiguration->PuttyPath : GUIConfiguration->PSftpPath),
              Data, (GUIConfiguration->PuttyPassword ? Data->Password : AnsiString()));
          }
          else
          {
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
