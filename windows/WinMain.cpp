//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <ScpMain.h>

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
TSessionData * GetLoginData(AnsiString SessionName, AnsiString & DownloadFile)
{
  bool DefaultsOnly;
  TSessionData * Data;

  Data = StoredSessions->ParseUrl(SessionName, DefaultsOnly,
    puExtractFileName | puDecodeUrlChars, &DownloadFile);
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
void __fastcall Upload(TTerminal * Terminal, TStrings * FileList)
{
  AnsiString TargetDirectory;
  TGUICopyParamType CopyParam = GUIConfiguration->DefaultCopyParam;

  TargetDirectory = UnixIncludeTrailingBackslash(Terminal->CurrentDirectory);

  int Options = coDisableQueue |
    (!Terminal->IsCapable[fcNewerOnlyUpload] ? coDisableNewerOnly : 0) |
    (!Terminal->IsCapable[fcTextMode] ? coDisableTransferMode : 0);
  if (DoCopyDialog(true, false, FileList, TargetDirectory, &CopyParam, Options))
  {
    int Params = (CopyParam.NewerOnly ? cpNewerOnly : 0);
    Terminal->CopyToRemote(FileList, TargetDirectory, &CopyParam, Params);
  }
}
//---------------------------------------------------------------------------
void __fastcall Download(TTerminal * Terminal, const AnsiString FileName)
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

    int Options = coDisableQueue |
      (!Terminal->IsCapable[fcTextMode] ? coDisableTransferMode : 0);
    if (DoCopyDialog(false, false, FileList, TargetDirectory, &CopyParam, Options))
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
  TStrings * CommandParams)
{
  AnsiString LocalDirectory;
  AnsiString RemoteDirectory;

  SynchronizeDirectories(Terminal, CommandParams, LocalDirectory, RemoteDirectory);

  bool SaveMode = true;
  // bit ugly
  TSynchronizeMode Mode = (TSynchronizeMode)GUIConfiguration->SynchronizeMode;
  if (ScpExplorer->DoFullSynchronizeDirectories(LocalDirectory,
        RemoteDirectory, Mode, SaveMode))
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
  TStrings * CommandParams)
{
  AnsiString LocalDirectory;
  AnsiString RemoteDirectory;

  SynchronizeDirectories(Terminal, CommandParams, LocalDirectory, RemoteDirectory);

  ScpExplorer->DoSynchronizeDirectories(LocalDirectory, RemoteDirectory);
  Abort();
}
//---------------------------------------------------------------------------
void __fastcall InvalidDefaultTranslation()
{
  if (WinConfiguration->ConfirmRemoveDefaultTranslation())
  {
    if (!DeleteFile(WinConfiguration->DefaultTranslationFile))
    {
      throw Exception(FMTLOAD(DELETE_LOCAL_FILE_ERROR,
        (WinConfiguration->DefaultTranslationFile)));
    }
  }
}
//---------------------------------------------------------------------------
int __fastcall Execute(TProgramParams * Params)
{
  assert(StoredSessions);
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
  if (Help || Params->FindSwitch("Console"))
  {
    return Console(Params, Help);
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

    AnsiString Value;

    if (Params->FindSwitch("UninstallCleanup"))
    {
      if (MessageDialog(LoadStr(UNINSTALL_CLEANUP), qtConfirmation,
            qaYes | qaNo, HELP_UNINSTALL_CLEANUP) == qaYes)
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
    else if (Params->FindSwitch("InvalidDefaultTranslation"))
    {
      InvalidDefaultTranslation();
    }
    else
    {
      TSessionData * Data;
      enum { pcNone, pcUpload, pcFullSynchronize, pcSynchronize } ParamCommand;
      ParamCommand = pcNone;
      AnsiString AutoStartSession;
      AnsiString DownloadFile;

      // do not check for temp dirs for service tasks (like RegisterAsUrlHandler)
      if (OnlyInstance &&
          WinConfiguration->TemporaryDirectoryCleanup)
      {
        TemporaryDirectoryCleanup();
      }

      WinConfiguration->CheckDefaultTranslation();

      if (Params->Count > 0)
      {
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

      Data = GetLoginData(AutoStartSession, DownloadFile);
      if (Data)
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
          CALLSTACK;
          if (TerminalManager->ConnectActiveTerminal())
          {
            CALLSTACK;
            TCustomScpExplorerForm * ScpExplorer = CreateScpExplorer();
            try
            {
              // moved inside try .. __finally, because it can fail as well
              TerminalManager->ScpExplorer = ScpExplorer;
              if (ParamCommand == pcUpload)
              {
                Upload(TerminalManager->ActiveTerminal, CommandParams);
              }
              else if (ParamCommand == pcFullSynchronize)
              {
                FullSynchronize(TerminalManager->ActiveTerminal, ScpExplorer,
                  CommandParams);
              }
              else if (ParamCommand == pcSynchronize)
              {
                Synchronize(TerminalManager->ActiveTerminal, ScpExplorer,
                  CommandParams);
              }
              else if (!DownloadFile.IsEmpty())
              {
                Download(TerminalManager->ActiveTerminal, DownloadFile);
              }

              CALLSTACK;
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

