//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <ScpMain.h>

#include <Log.h>
#include <TextsWin.h>
#include <TextsCore.h>

#include "CustomScpExplorer.h"
#include "TerminalManager.h"
#include "NonVisual.h"
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
void __fastcall Upload(TTerminal * Terminal, TProgramParams * Params,
  int ListFrom, int ListTo)
{
  AnsiString TargetDirectory;
  TGUICopyParamType CopyParam = GUIConfiguration->CopyParam;
  TStrings * FileList = NULL;

  try
  {
    FileList = new TStringList();
    for (int Index = ListFrom; Index <= ListTo; Index++)
    {
      FileList->Add(Params->Param[Index]);
    }
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
  __finally
  {
    delete FileList;
  }
}
//---------------------------------------------------------------------------
void __fastcall Download(TTerminal * Terminal, const AnsiString FileName)
{
  AnsiString TargetDirectory;
  TGUICopyParamType CopyParam = GUIConfiguration->CopyParam;
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
  TProgramParams * Params, int ParamStart,
  AnsiString & LocalDirectory, AnsiString & RemoteDirectory)
{
  if (ParamStart <= Params->ParamCount)
  {
    LocalDirectory = Params->Param[ParamStart];
  }
  else if (!Terminal->SessionData->LocalDirectory.IsEmpty())
  {
    LocalDirectory = Terminal->SessionData->LocalDirectory;
  }
  else
  {
    LocalDirectory = WinConfiguration->ScpExplorer.LastLocalTargetDirectory;
  }

  if (ParamStart + 1 <= Params->ParamCount)
  {
    RemoteDirectory = Params->Param[ParamStart + 1];
  }
  else
  {
    RemoteDirectory = Terminal->CurrentDirectory;
  }
}
//---------------------------------------------------------------------------
void __fastcall FullSynchronize(TTerminal * Terminal, TCustomScpExplorerForm * ScpExplorer,
  TProgramParams * Params, int ParamStart)
{
  AnsiString LocalDirectory;
  AnsiString RemoteDirectory;

  SynchronizeDirectories(Terminal, Params, ParamStart, LocalDirectory, RemoteDirectory);

  TSynchronizeMode Mode = smRemote;
  if (ScpExplorer->DoFullSynchronizeDirectories(LocalDirectory,
        RemoteDirectory, Mode))
  {
    Terminal->CloseOnCompletion();
  }
  else
  {
    Abort();
  }
}
//---------------------------------------------------------------------------
void __fastcall Synchronize(TTerminal * Terminal, TCustomScpExplorerForm * ScpExplorer,
  TProgramParams * Params, int ParamStart)
{
  AnsiString LocalDirectory;
  AnsiString RemoteDirectory;

  SynchronizeDirectories(Terminal, Params, ParamStart, LocalDirectory, RemoteDirectory);

  ScpExplorer->DoSynchronizeDirectories(LocalDirectory, RemoteDirectory);
  Abort();
}
//---------------------------------------------------------------------------
void __fastcall InvalidDefaultTranslation(const AnsiString FileName)
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

  // let installer know, that some instance of application is running
  CreateMutex(NULL, False, AppName.c_str());
  bool OnlyInstance = (GetLastError() == 0);

  bool Help = Params->FindSwitch("help") || Params->FindSwitch("h") || Params->FindSwitch("?");
  if (Help || Params->FindSwitch("Console"))
  {
    return Console(Params, Help);
  }

  TTerminalManager * TerminalManager = NULL;
  NonVisualDataModule = NULL;
  try
  {
    TerminalManager = TTerminalManager::Instance();
    NonVisualDataModule = new TNonVisualDataModule(Application);

    LogForm = NULL;

    Application->HintHidePause = 1000;

    if (OnlyInstance &&
        WinConfiguration->TemporaryDirectoryCleanup)
    {
      TemporaryDirectoryCleanup();
    }

    AnsiString Value;

    if (Params->FindSwitch("UninstallCleanup"))
    {
      if (MessageDialog(LoadStr(UNINSTALL_CLEANUP), qtConfirmation,
            qaYes | qaNo, 0) == qaYes)
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
      CheckForUpdates();
    }
    else if (Params->FindSwitch("InvalidDefaultTranslation", Value))
    {
      InvalidDefaultTranslation(Value);
    }
    else
    {
      TSessionData * Data;
      enum { pcNone, pcUpload, pcFullSynchronize, pcSynchronize } ParamCommand;
      ParamCommand = pcNone;
      int CommandParamsStart;
      AnsiString AutoStartSession;
      AnsiString DownloadFile;

      WinConfiguration->CheckDefaultTranslation();

      if (Params->Count > 0)
      {
        AnsiString DummyValue;
        if (Params->FindSwitch("Upload", DummyValue, CommandParamsStart))
        {
          ParamCommand = pcUpload;
          if (CommandParamsStart == Params->ParamCount)
          {
            throw Exception(NO_UPLOAD_LIST_ERROR);
          }
        }
        else if (Params->FindSwitch("Synchronize", DummyValue, CommandParamsStart))
        {
          ParamCommand = pcFullSynchronize;
        }
        else if (Params->FindSwitch("KeepUpToDate", DummyValue, CommandParamsStart))
        {
          ParamCommand = pcSynchronize;
        }
        else if (Params->ParamCount > 0)
        {
          AutoStartSession = Params->Param[1];
        }

        if (ParamCommand != pcNone)
        {
          CommandParamsStart++;
          if (CommandParamsStart >= 2)
          {
            AutoStartSession = Params->Param[1];
          }
        }
      }
      else if (WinConfiguration->EmbeddedSessions && StoredSessions->Count)
      {
        AutoStartSession = StoredSessions->Sessions[0]->Name;
      }
      else
      {
        AutoStartSession = WinConfiguration->AutoStartSession;
      }

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
          if (TerminalManager->ConnectActiveTerminal())
          {
            TCustomScpExplorerForm * ScpExplorer = CreateScpExplorer();
            try
            {
              // moved inside try .. __finally, because it can fail as well
              TerminalManager->ScpExplorer = ScpExplorer;
              if (ParamCommand == pcUpload)
              {
                if (CommandParamsStart <= Params->ParamCount)
                {
                  Upload(TerminalManager->ActiveTerminal, Params,
                    CommandParamsStart, Params->ParamCount);
                }
                else
                {
                  throw Exception(NO_UPLOAD_LIST_ERROR);
                }
              }
              else if (ParamCommand == pcFullSynchronize)
              {
                FullSynchronize(TerminalManager->ActiveTerminal, ScpExplorer,
                  Params, CommandParamsStart);
              }
              else if (ParamCommand == pcSynchronize)
              {
                Synchronize(TerminalManager->ActiveTerminal, ScpExplorer,
                  Params, CommandParamsStart);
              }
              else if (!DownloadFile.IsEmpty())
              {
                Download(TerminalManager->ActiveTerminal, DownloadFile);
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
  __finally
  {
    delete NonVisualDataModule;
    NonVisualDataModule = NULL;
    TTerminalManager::DestroyInstance();
  }

  return 0;
}

