//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <stdio.h>

#include <Interface.h>
#include <Common.h>
#include <Configuration.h>
#include <ScpMain.h>
#include <Terminal.h>

#include <Log.h>
#include <TextsWin.h>

#include "CustomScpExplorer.h"
#include "TerminalManager.h"
#include "NonVisual.h"
#include "ProgParams.h"
#include "Tools.h"
#include "WinConfiguration.h"

#include <NMHttp.hpp>
#include <Psock.hpp>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
TSessionData * GetLoginData(AnsiString SessionName)
{
  bool ProtocolDefined = false;
  TFSProtocol Protocol;
  if (SessionName.SubString(1, 6).LowerCase() == "scp://")
  {
    Protocol = fsSCPonly;
    SessionName.Delete(1, 6);
    ProtocolDefined = true;
  }
  else if (SessionName.SubString(1, 7).LowerCase() == "sftp://")
  {
    Protocol = fsSFTPonly;
    SessionName.Delete(1, 7);
    ProtocolDefined = true;
  }

  bool DefaultsOnly = true;
  TSessionData *Data = new TSessionData("");
  if (!SessionName.IsEmpty())
  {
    TSessionData * AData = NULL;
    if (!ProtocolDefined)
    {
      AData = (TSessionData *)StoredSessions->FindByName(SessionName, False);
    }
    
    if (!AData)
    {
      Data->Assign(StoredSessions->DefaultSettings);
      if (Data->ParseUrl(SessionName, 0))
      {
        Data->Name = "";
        DefaultsOnly = false;
      }
      else
      {
        SimpleErrorDialog(FMTLOAD(SESSION_NOT_EXISTS_ERROR, (SessionName)));
      }
    }
    else
    {
      DefaultsOnly = false;
      Data->Assign(AData);
      if (StoredSessions->IsHidden(AData))
      {
        AData->Remove();
        StoredSessions->Remove(AData);
        StoredSessions->Save();
      }
    }
  }
  else
  {
    Data->Assign(StoredSessions->DefaultSettings);
  }

  if (ProtocolDefined)
  {
    Data->FSProtocol = Protocol;
  }

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
  TCopyParamType CopyParam = Configuration->CopyParam;
  TStrings * FileList = NULL;

  try
  {
    FileList = new TStringList();
    for (int Index = ListFrom; Index <= ListTo; Index++)
    {
      FileList->Add(Params->Param[Index]);
    }
    TargetDirectory = UnixIncludeTrailingBackslash(Terminal->CurrentDirectory);

    if (DoCopyDialog(true, false, false, FileList,
          Terminal->IsCapable[fcTextMode], TargetDirectory, &CopyParam, true))
    {
      int Params = 0;
      Terminal->CopyToRemote(FileList, TargetDirectory, &CopyParam, Params);
    }
  }
  __finally
  {
    delete FileList;
  }
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

      TNMHTTP * CheckForUpdatesHTTP = new TNMHTTP(Application);
      try
      {
        CheckForUpdatesHTTP->Get(LoadStr(UPDATES_URL));
        Response = CheckForUpdatesHTTP->Body;
      }
      __finally
      {
        delete CheckForUpdatesHTTP;
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
void __fastcall Execute(TProgramParams * Params)
{
  assert(StoredSessions);
  assert(Params);

  TTerminalManager * TerminalManager = NULL;
  NonVisualDataModule = NULL;
  try
  {
    TerminalManager = TTerminalManager::Instance();
    NonVisualDataModule = new TNonVisualDataModule(Application);

    LogForm = NULL;

    Application->HintHidePause = 1000;

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
      int UploadListStart = 0;
      AnsiString AutoStartSession;

      if (Params->ParamCount)
      {
        AnsiString DummyValue;
        if (Params->FindSwitch("Upload", DummyValue, UploadListStart))
        {
          UploadListStart++;
          if (UploadListStart >= 2)
          {
            AutoStartSession = Params->Param[1];
          }
        }
        else
        {
          AutoStartSession = Params->Param[1];
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

      Data = GetLoginData(AutoStartSession);
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
              if (UploadListStart > 0)
              {
                if (UploadListStart <= Params->ParamCount)
                {
                  Upload(TerminalManager->ActiveTerminal, Params,
                    UploadListStart, Params->ParamCount);
                }
                else
                {
                  throw Exception(NO_UPLOAD_LIST_ERROR);
                }
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
          ShowExtendedExceptionEx(&E, Application, true);
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

