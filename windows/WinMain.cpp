//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <stdio.h>

#include <Interface.h>
#include <Common.h>
#include <Configuration.h>
#include <ScpMain.h>
#include <Terminal.h>

#include <OperationStatus.h>
#include <Log.h>
#include <TextsWin.h>

#include "CustomScpExplorer.h"
#include "UserInterface.h"
#include "EventHandler.h"
#include "NonVisual.h"
#include "ProgParams.h"
#include "Tools.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
TLogMemo *LogMemo;
static TTerminal *Terminal;
static TCustomScpExplorerForm *ScpExplorer = NULL;
//---------------------------------------------------------------------------
void __fastcall Connect(TTerminal *Terminal)
{
  TCursor OldCursor;

  Application->Title =
    FMTLOAD(APP_CAPTION, (Terminal->SessionData->SessionName, AppName));
  TOperationStatusForm *Form = new TOperationStatusForm(Application);
  OldCursor = Screen->Cursor;
  Screen->Cursor = crAppStart;
  try
  {
    Form->SecureShell = Terminal;
    Form->Show();
    Terminal->Open();
    Terminal->DoStartup();
  }
  __finally
  {
    Screen->Cursor = OldCursor;
    delete Form;
  }
}
//---------------------------------------------------------------------------
TLogMemo * __fastcall CreateLogMemo()
{
  TLogMemo * aLogMemo = new TLogMemo(Application);
  try {
    aLogMemo->SessionLog = Terminal->Log;
    aLogMemo->PopupMenu = NonVisualDataModule->LogMemoPopup;
  } catch (...) {
    delete aLogMemo;
    throw;
  }
  LogMemo = aLogMemo;
  return aLogMemo;
}
//---------------------------------------------------------------------------
void __fastcall FreeLogMemo()
{
  LogMemo->PopupMenu = NULL;
  SAFE_DESTROY(LogMemo);
}
//---------------------------------------------------------------------------
TSessionData * GetLoginData(const AnsiString SessionName)
{
  bool DefaultsOnly = true;
  TSessionData *Data = new TSessionData("");
  if (!SessionName.IsEmpty())
  {
    TSessionData * AData;
    AData = (TSessionData *)StoredSessions->FindByName(SessionName, False);
    if (!AData)
    {
      Data->Assign(StoredSessions->DefaultSettings);
      // 2.0 #89   2002-2-21
      // if command line parameter is not name of any stored session,
      // we check if its hase "username:password@host" format
      Integer AtPos = SessionName.Pos("@");
      if (AtPos)
      {
        DefaultsOnly = false;
        AnsiString Param = SessionName;
        Data->HostName = Param.SubString(AtPos+1, Param.Length() - AtPos);
        Param.SetLength(AtPos - 1);
        Integer ColonPos = Param.Pos(":");
        if (ColonPos)
        {
          Data->Password = Param.SubString(ColonPos + 1, Param.Length() - ColonPos);
          Param.SetLength(ColonPos - 1);
        }
        Data->UserName = Param;
      }
        else
      SimpleErrorDialog(FMTLOAD(SESSION_NOT_EXISTS_ERROR, (SessionName)));
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
  if (!Data->CanLogin || DefaultsOnly)
  {
    if (!DoLoginDialog(StoredSessions, Data) || !Data->CanLogin)
    {
      delete Data;
      Data = NULL;
    }
  }
  return Data;
}
//---------------------------------------------------------------------------
/*void __fastcall OpenScpExplorer()
{
  ScpExplorer = NULL;
  try {
    ScpExplorer = CreateScpExplorer();
    assert(ScpExplorer);
    ScpExplorer->Icon->Assign(Application->Icon);

    EventHandler->ScpExplorer = ScpExplorer;
    ScpExplorer->Terminal = Terminal;

    Application->Run();

    if (Terminal->Active &&
        !Terminal->SessionData->Modified)
    {
      ScpExplorer->UpdateSessionData();
      TSessionData * Data;
      Data = (TSessionData *)StoredSessions->FindByName(
        Terminal->SessionData->Name);
      if (Data)
      {
        Data->Assign(Terminal->SessionData);
        StoredSessions->Save();
      }
    }

  } __finally {
    EventHandler->ScpExplorer = NULL;
    SAFE_DESTROY(ScpExplorer);
  }
} */
//---------------------------------------------------------------------------
void __fastcall Upload(TProgramParams * Params, int ListFrom, int ListTo)
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

    if (DoCopyDialog(tdToRemote, ttCopy, false, FileList,
         (Terminal->SessionData->EOLType != Configuration->LocalEOLType),
         TargetDirectory, &CopyParam))
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
void __fastcall Execute(TProgramParams * Params)
{
  assert(StoredSessions);
  assert(Params);

  TEventHandler * EventHandler = NULL;

  NonVisualDataModule = NULL;
  try {
    EventHandler = new TEventHandler();
    NonVisualDataModule = new TNonVisualDataModule(Application);

    LogMemo = NULL;
    LogForm = NULL;
    Terminal = NULL;

    Application->HintHidePause = 1000;

    if (Params->FindSwitch("RandomSeedFileCleanup"))
    {
      Configuration->CleanupRandomSeedFile();
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
        else
      if (Configuration->EmbeddedSessions && StoredSessions->Count)
      {
        AutoStartSession = StoredSessions->Sessions[0]->Name;
      }
        else
      {
        AutoStartSession = Configuration->AutoStartSession;
      }

      Data = GetLoginData(AutoStartSession);
      if (Data)
      {
        try {
          Configuration->OnChange = EventHandler->ConfigurationChange;
          Boolean ShowLogPending = False;
          Terminal = new TTerminal();
          CreateLogMemo();

          Terminal->Configuration = Configuration;
          Terminal->SessionData = Data;
          Terminal->OnQueryUser = EventHandler->TerminalQueryUser;

          try {
            if (Configuration->Logging && (Configuration->LogView == lvWindow))
            {
              if (Configuration->LogWindowOnStartup) RequireLogForm(LogMemo);
                else ShowLogPending = True;
            }

            try {
              Connect(Terminal);

              Configuration->ClearTemporaryLoginData();

              if (LogForm && (Configuration->LogView != lvWindow))
                FreeLogForm();

              if (ShowLogPending) RequireLogForm(LogMemo);

              ScpExplorer = NULL;
              try {
                ScpExplorer = CreateScpExplorer();
                assert(ScpExplorer);
                ScpExplorer->Icon->Assign(Application->Icon);

                EventHandler->ScpExplorer = ScpExplorer;
                ScpExplorer->Terminal = Terminal;

                if (UploadListStart > 0)
                {
                  if (UploadListStart <= Params->ParamCount)
                  {
                    Upload(Params, UploadListStart, Params->ParamCount);
                  }
                  else
                  {
                    throw Exception(NO_UPLOAD_LIST_ERROR);
                  }
                }
                Application->Run();

                if (Terminal->Active &&
                    !Terminal->SessionData->Modified)
                {
                  ScpExplorer->UpdateSessionData();
                  TSessionData * Data;
                  Data = (TSessionData *)StoredSessions->FindByName(
                    Terminal->SessionData->Name);
                  if (Data)
                  {
                    Data->Assign(Terminal->SessionData);
                    StoredSessions->Save();
                  }
                }

              } __finally {
                EventHandler->ScpExplorer = NULL;
                SAFE_DESTROY(ScpExplorer);
              }

            } __finally {
              FreeLogForm();
            }
          } catch (Exception &E) {
            ShowExtendedException(&E, Application);
          }
        } __finally {
          delete Data;
          Configuration->OnChange = NULL;
          SAFE_DESTROY(Terminal);
          FreeLogMemo();
        }
      }
    }
  } __finally {
    delete EventHandler;
    EventHandler = NULL;
    delete NonVisualDataModule;
    NonVisualDataModule = NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall ReconnectTerminal()
{
  assert(Terminal);
  AnsiString SessionName;
  if (!Configuration->EmbeddedSessions)
  {
    SessionName = StoredSessions->HiddenPrefix+"reconnect";
    TSessionData * Data = new TSessionData("");
    try {
      Data->Assign(Terminal->SessionData);
      if (ScpExplorer)
      {
        ScpExplorer->UpdateSessionData(Data);
        ScpExplorer->StoreParams();
      }
      StoredSessions->NewSession(SessionName, Data);
    } __finally {
      delete Data;
    }
  }
  else
  {
    SessionName = Terminal->SessionData->Name;
  }

  Configuration->Save();
  Configuration->DontSave = true;
  if (!ExecuteShell(Application->ExeName, SessionName))
    throw Exception(FORMAT(RECONNECT_ERROR, (AppName)));
}
