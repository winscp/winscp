//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <stdio.h>

#include <Interface.h>
#include <Common.h>
#include <Configuration.h>
#include <ScpMain.h>
#include <Terminal.h>
#include <Net.h>

#include <Log.h>
#include <TextsWin.h>
#include <TextsCore.h>

#include "CustomScpExplorer.h"
#include "TerminalManager.h"
#include "NonVisual.h"
#include "ProgParams.h"
#include "Tools.h"
#include "WinConfiguration.h"
#include "GUITools.h"

#include <TcpIp.hpp>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
TSessionData * GetLoginData(AnsiString SessionName, AnsiString & DownloadFile)
{
  bool DefaultsOnly;
  TSessionData * Data;

  Data = StoredSessions->ParseUrl(SessionName, DefaultsOnly,
    puExtractFileName, &DownloadFile);
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
int __fastcall CalculateCompoundVersion(int MajorVer,
  int MinorVer, int Release, int Build)
{
  int CompoundVer = Build + 1000 * (Release + 100 * (MinorVer +
    100 * MajorVer));
  return CompoundVer;
}
//---------------------------------------------------------------------------
void __fastcall RegisterAsUrlHandler()
{
  try
  {
    bool Success;
    bool User = true;
    TRegistry * Registry = new TRegistry();
    try
    {
      do
      {
        Success = true;
        User = !User;

        try
        {
          assert(Configuration != NULL);
          AnsiString FileName = Application->ExeName;
          AnsiString BaseKey;

          Registry->Access = KEY_WRITE;
          if (User)
          {
            Registry->RootKey = HKEY_CURRENT_USER;
            BaseKey = "Software\\Classes\\";
          }
          else
          {
            Registry->RootKey = HKEY_CLASSES_ROOT;
            BaseKey = "";
          }

          AnsiString Protocol;
          for (int Index = 0; Index <= 1; Index++)
          {
            Protocol = (Index == 0) ? "SCP" : "SFTP";
            if (Registry->OpenKey(BaseKey + Protocol, true))
            {
              Registry->WriteString("", FMTLOAD(PROTOCOL_URL_DESC, (Protocol)));
              Registry->WriteString("URL Protocol", "");
              Registry->WriteInteger("EditFlags", 0x02);
              Registry->WriteInteger("BrowserFlags", 0x08);
              if (Registry->OpenKey("DefaultIcon", true))
              {
                Registry->WriteString("", FORMAT("\"%s\",0", (FileName)));
                Registry->CloseKey();
              }
              else
              {
                Abort();
              }
            }
            else
            {
              Abort();
            }

            if (Registry->OpenKey(BaseKey + Protocol, false) &&
                Registry->OpenKey("shell", true) &&
                Registry->OpenKey("open", true) &&
                Registry->OpenKey("command", true))
            {
              Registry->WriteString("", FORMAT("\"%s\" %%1", (FileName)));
              Registry->CloseKey();
            }
            else
            {
              Abort();
            }
          }
        }
        catch(...)
        {
          Success = false;
        }
      }
      while (!Success && !User);
    }
    __finally
    {
      delete Registry;
    }
  }
  catch(Exception & E)
  {
    throw ExtException(&E, LoadStr(REGISTER_URL_ERROR));
  }
}
//---------------------------------------------------------------------------
void __fastcall CheckForUpdates()
{
  bool Found = false;
  TCustomForm * ActiveForm = Screen->ActiveCustomForm;
  Busy(true);
  try
  {
    if (ActiveForm)
    {
      assert(ActiveForm->Enabled);
      ActiveForm->Enabled = false;
    }

    try
    {
      AnsiString Response;

      if (SessionsCount == 0)
      {
        NetInitialize();
      }

      THttp * CheckForUpdatesHTTP = new THttp(Application);
      try
      {
        CheckForUpdatesHTTP->URL = LoadStr(UPDATES_URL);
        CheckForUpdatesHTTP->Action();
        Response.SetLength(static_cast<int>(CheckForUpdatesHTTP->Stream->Size));
        CheckForUpdatesHTTP->Stream->Read(Response.c_str(), Response.Length());
      }
      __finally
      {
        delete CheckForUpdatesHTTP;
        if (SessionsCount == 0)
        {
          NetFinalize();
        }
      }

      while (!Response.IsEmpty() && !Found)
      {
        AnsiString Line = ::CutToChar(Response, '\n', false);
        AnsiString Name = ::CutToChar(Line, '=', false);
        if (AnsiSameText(Name, "Version"))
        {
          Found = true;
          int MajorVer = StrToInt(::CutToChar(Line, '.', false));
          int MinorVer = StrToInt(::CutToChar(Line, '.', false));
          int Release = StrToInt(::CutToChar(Line, '.', false));
          int Build = StrToInt(::CutToChar(Line, '.', false));
          int CompoundVer = CalculateCompoundVersion(MajorVer, MinorVer, Release, Build);

          AnsiString VersionStr =
            FORMAT("%d.%d", (MajorVer, MinorVer)) + (Release ? "."+IntToStr(Release) : AnsiString());

          TVSFixedFileInfo * FileInfo = Configuration->FixedApplicationInfo;
          int CurrentCompoundVer = CalculateCompoundVersion(
            HIWORD(FileInfo->dwFileVersionMS), LOWORD(FileInfo->dwFileVersionMS),
            HIWORD(FileInfo->dwFileVersionLS), LOWORD(FileInfo->dwFileVersionLS));

          if (CurrentCompoundVer < CompoundVer)
          {
            if (MessageDialog(FMTLOAD(NEW_VERSION, (VersionStr)), qtInformation,
                  qaOK | qaCancel, 0) == qaOK)
            {
              NonVisualDataModule->OpenBrowser(LoadStr(DOWNLOAD_URL));
            }
          }
          else
          {
            MessageDialog(LoadStr(NO_NEW_VERSION), qtInformation, qaOK, 0);
          }
        }
      }

    }
    catch(Exception & E)
    {
      throw ExtException(&E, LoadStr(CHECK_FOR_UPDATES_ERROR));
    }
  }
  __finally
  {
    if (ActiveForm)
    {
      ActiveForm->Enabled = true;
    }
    Busy(false);
  }

  if (!Found)
  {
    throw Exception(LoadStr(CHECK_FOR_UPDATES_ERROR));
  }
}
//---------------------------------------------------------------------------
void __fastcall TemporaryDirectoryCleanup()
{
  bool Continue = true;
  TStrings * Folders = NULL;
  try
  {
    if (WinConfiguration->ConfirmTemporaryDirectoryCleanup)
    {
      Folders = WinConfiguration->FindTemporaryFolders();
      Continue = (Folders != NULL);

      if (Continue)
      {
        TQueryButtonAlias Aliases[1];
        Aliases[0].Button = qaRetry;
        Aliases[0].Alias = LoadStr(OPEN_BUTTON);
        TMessageParams Params(mpNeverAskAgainCheck);
        Params.Aliases = Aliases;
        Params.AliasesCount = LENOF(Aliases);

        int Answer = MoreMessageDialog(
          FMTLOAD(CLEAN_TEMP_CONFIRM, (Folders->Count)), Folders,
          qtWarning, qaYes | qaNo | qaRetry, 0, &Params);

        if (Answer == qaNeverAskAgain)
        {
          WinConfiguration->ConfirmTemporaryDirectoryCleanup = false;
          Answer = qaYes;
        }
        else if (Answer == qaRetry)
        {
          for (int Index = 0; Index < Folders->Count; Index++)
          {
            ShellExecute(Application->Handle, NULL,
              Folders->Strings[Index].c_str(), NULL, NULL, SW_SHOWNORMAL);
          }
        }
        Continue = (Answer == qaYes);
      }
    }

    if (Continue)
    {
      TStrings * F = Folders;
      Folders = NULL;
      try
      {
        WinConfiguration->CleanupTemporaryFolders(F);
      }
      catch (Exception &E)
      {
        ShowExtendedException(&E);
      }
    }
  }
  __finally
  {
    delete Folders;
  }
}
//---------------------------------------------------------------------------
void __fastcall Execute(TProgramParams * Params)
{
  assert(StoredSessions);
  assert(Params);

  // let installer know, that some instance of application is running
  CreateMutex(NULL, False, AppName.c_str());
  bool OnlyInstance = (GetLastError() == 0);

  bool Help = Params->FindSwitch("help") || Params->FindSwitch("h") || Params->FindSwitch("?");
  if (Help || Params->FindSwitch("Console"))
  {
    Console(Params, Help);
    return;
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

    if (Params->FindSwitch("UninstallCleanup"))
    {
      if (MessageDialog(LoadStr(UNINSTALL_CLEANUP), qtConfirmation,
            qaOK | qaCancel, 0) == qaOK)
      {
        DoCleanupDialog(StoredSessions, Configuration);
      }
    }
    else if (Params->FindSwitch("RegisterAsUrlHandler"))
    {
      RegisterAsUrlHandler();
    }
    else if (Params->FindSwitch("Update"))
    {
      CheckForUpdates();
    }
    else
    {
      TSessionData * Data;
      enum { pcNone, pcUpload, pcFullSynchronize, pcSynchronize } ParamCommand;
      ParamCommand = pcNone;
      int CommandParamsStart;
      AnsiString AutoStartSession;
      AnsiString DownloadFile;

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
        else
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
}

