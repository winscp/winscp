//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "TerminalManager.h"
#include "CustomScpExplorer.h"
#include "LogMemo.h"
#include "NonVisual.h"
#include "WinConfiguration.h"
#include <Log.h>
#include <OperationStatus.h>
#include <Common.h>
#include <ScpMain.h>
#include <TextsWin.h>
#include <Progress.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
TTerminalManager * TTerminalManager::FInstance = NULL;
//---------------------------------------------------------------------------
TTerminalManager * __fastcall TTerminalManager::Instance(bool ForceCreation)
{
  if (!FInstance && ForceCreation)
  {
    FInstance = new TTerminalManager();
  }
  return FInstance;
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::DestroyInstance()
{
  assert(FInstance);
  SAFE_DESTROY(FInstance);
}
//---------------------------------------------------------------------------
__fastcall TTerminalManager::TTerminalManager() :
  TTerminalList(Configuration)
{
  FLogMemo = NULL;
  FActiveTerminal = NULL;
  FScpExplorer = NULL;
  FDestroying = false;
  FTerminalPendingAction = tpNull;
  FProgress = -1;

  assert(Application && !Application->OnException);
  Application->OnException = ApplicationException;
  
  assert(Configuration && !Configuration->OnChange);
  Configuration->OnChange = ConfigurationChange;
  FOnLastTerminalClosed = NULL;
  FOnTerminalListChanged = NULL;

  FTerminalList = new TStringList();
}
//---------------------------------------------------------------------------
__fastcall TTerminalManager::~TTerminalManager()
{
  FreeAll();

  assert(!ScpExplorer);

  assert(Configuration->OnChange == ConfigurationChange);
  Configuration->OnChange = NULL;

  assert(Application && (Application->OnException == ApplicationException));
  Application->OnException = NULL;

  delete FTerminalList;
}
//---------------------------------------------------------------------------
TTerminal * __fastcall TTerminalManager::NewTerminal(TSessionData * Data)
{
  FTerminalList->Clear();
  TTerminal * Terminal = TTerminalList::NewTerminal(Data);
  try
  {
    Terminal->OnQueryUser = TerminalQueryUser;
    Terminal->OnProgress = OperationProgress;
    Terminal->OnFinished = OperationFinished;
    Terminal->OnDeleteLocalFile = DeleteLocalFile; 
    if (!ActiveTerminal)
    {
      ActiveTerminal = Terminal;
    }
  }
  catch(...)
  {
    FreeTerminal(Terminal);
    throw;
  }
  if (OnTerminalListChanged)
  {
    OnTerminalListChanged(this);
  }
  return Terminal;
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::FreeActiveTerminal()
{
  if (FTerminalPendingAction == tpNull)
  {
    assert(ActiveTerminal);
    FreeTerminal(ActiveTerminal);
  }
  else
  {
    assert(FTerminalPendingAction == tpNone);
    FTerminalPendingAction = tpFree;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminalManager::ConnectActiveTerminal()
{
  TTerminalPendingAction Action;
  bool Result;
  do
  {
    Action = tpNull;
    Result = false;
    try
    {
      assert(ActiveTerminal);
      bool ShowLogPending = false;

      if (Configuration->Logging && (WinConfiguration->LogView == lvWindow))
      {
        if (WinConfiguration->LogWindowOnStartup)
        {
          RequireLogForm(LogMemo);
        }
        else
        {
          ShowLogPending = true;
        }
      }

      TOperationStatusForm * Form = new TOperationStatusForm(Application);
      Busy(true);
      try
      {
        Form->SecureShell = ActiveTerminal;
        Form->ShowAsModal();
        ActiveTerminal->Open();
        ActiveTerminal->DoStartup();
      }
      __finally
      {
        Busy(false);
        delete Form;
      }

      if (ScpExplorer)
      {
        assert(ActiveTerminal->Status == sshReady);
        TerminalReady();
      }

      WinConfiguration->ClearTemporaryLoginData();

      if (LogForm && (WinConfiguration->LogView != lvWindow))
      {
        FreeLogForm();
      }
  
      if (ShowLogPending)
      {
        RequireLogForm(LogMemo);
      }
      Result = true;
    }
    catch(Exception & E)
    {
      assert(FTerminalPendingAction == tpNull);
      FTerminalPendingAction = tpNone;
      try
      {
        ShowExtendedException(&E, this);
        Action = FTerminalPendingAction;
      }
      __finally
      {
        FTerminalPendingAction = tpNull;
      }
    }
  }
  while (Action == tpReconnect);

  if (Action == tpFree)
  {
    FreeActiveTerminal();
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::ReconnectActiveTerminal()
{
  assert(ActiveTerminal);

  TTerminal * Terminal;
  if (ScpExplorer)
  {
    TSessionData * Data = new TSessionData(ActiveTerminal->SessionData->Name);
    try
    {
      Data->Assign(ActiveTerminal->SessionData);
      if (ScpExplorer->Terminal == ActiveTerminal)
      {
        ScpExplorer->UpdateSessionData(Data);
      }
      Terminal = NewTerminal(Data);
    }
    __finally
    {
      delete Data;
    }
  }
  else
  {
    // otherwise the ScpExplorer would be created already
    assert(ActiveTerminal->Status < sshReady);
    Terminal = NewTerminal(ActiveTerminal->SessionData);
  }

  try
  {
    FreeTerminal(ActiveTerminal);
    ActiveTerminal = Terminal;
    if (FTerminalPendingAction == tpNull)
    {
      ConnectActiveTerminal();
    }
    else
    {
      FTerminalPendingAction = tpReconnect;
    }
  }
  catch(...)
  {
    FreeTerminal(Terminal);
    throw;
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::FreeAll()
{
  FDestroying = true;
  try
  {
    while (Count)
    {
      FreeTerminal(Terminals[0]);
    }
  }
  __finally
  {
    FDestroying = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::FreeTerminal(TTerminal * Terminal)
{
  try
  {
    Terminal->Active = false;
  }
  __finally
  {
    int Index = IndexOf(Terminal);
    FTerminalList->Clear();
    Extract(Terminal);
    if (ActiveTerminal && (Terminal == ActiveTerminal))
    {
      if ((Count > 0) && !FDestroying)
      {
        for (int i = 0; i < Count; i++)
        {
          if (Terminals[i]->Status == sshReady)
          {
            ActiveTerminal = Terminals[i];
            break;
          }
        }
        if (ActiveTerminal == Terminal)
        {
          ActiveTerminal = Terminals[Index];
        }
      }
      else
      {
        ActiveTerminal = NULL;
      }
    }
    else
    {
      SaveTerminal(Terminal);
    }

    delete Terminal;

    if (OnTerminalListChanged)
    {
      OnTerminalListChanged(this);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::SetScpExplorer(TCustomScpExplorerForm * value)
{
  if (ScpExplorer != value)
  {
    // changing explorer is not supported yet
    assert(!ScpExplorer || !value);
    FScpExplorer = value;
    if (FScpExplorer)
    {
      assert(!OnChangeTerminal);
      FScpExplorer->Terminal = ActiveTerminal;
      FOnLastTerminalClosed = FScpExplorer->LastTerminalClosed;
      FOnTerminalListChanged = FScpExplorer->TerminalListChanged;
    }
    else
    {
      FOnLastTerminalClosed = NULL;
      FOnTerminalListChanged = NULL;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::SetActiveTerminal(TTerminal * value)
{
  if (ActiveTerminal != value)
  {
    if (ActiveTerminal && ScpExplorer)
    {
      assert(!ScpExplorer->Terminal || (ScpExplorer->Terminal == ActiveTerminal));
      if (ScpExplorer->Terminal == ActiveTerminal)
      {
        ScpExplorer->UpdateSessionData();
      }
    }

    TTerminal * PActiveTerminal = ActiveTerminal;
    FActiveTerminal = NULL;
    if (OnChangeTerminal)
    {
      OnChangeTerminal(this);
    }
    FActiveTerminal = value;
    // moved from else block of next if (ActiveTerminal) statement
    // so ScpExplorer can update its caption
    UpdateAppTitle();
    if (ScpExplorer)
    {
      if (ActiveTerminal && (ActiveTerminal->Status == sshReady))
      {
        TerminalReady();
      }
      else
      {
        ScpExplorer->Terminal = NULL;
      }
    }

    if (PActiveTerminal && !PActiveTerminal->Active)
    {
      SaveTerminal(PActiveTerminal);
    }

    if (ActiveTerminal)
    {
      if (!PActiveTerminal)
      {
        CreateLogMemo();
      }
      assert(LogMemo);
      LogMemo->SessionLog = ActiveTerminal->Log;
    }
    else
    {
      if (LogForm)
      {
        FreeLogForm();
      }
      FreeLogMemo();
      if (OnLastTerminalClosed)
      {
        OnLastTerminalClosed(this);
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::UpdateAppTitle()
{
  AnsiString NewTitle;
  if (ActiveTerminal)
  {
    NewTitle = FMTLOAD(APP_CAPTION, (ActiveTerminalTitle, AppName));
  }
  else
  {
    NewTitle = AppName;
  }

  if (FProgress >= 0)
  {
    NewTitle = FORMAT("%d%% %s - %s",
      (FProgress, TProgressForm::OperationName(FOperation), NewTitle));
  }
  
  Application->Title = NewTitle;
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::SaveTerminal(TTerminal * Terminal)
{
  if (!Terminal->SessionData->Name.IsEmpty())
  {
    TSessionData * Data;
    Data = (TSessionData *)StoredSessions->FindByName(Terminal->SessionData->Name);
    if (Data)
    {
      bool Changed = false;
      if (Terminal->SessionData->UpdateDirectories)
      {
        Data->LocalDirectory = Terminal->SessionData->LocalDirectory;
        Data->RemoteDirectory = Terminal->SessionData->RemoteDirectory;
        Changed = true;
      }

      if (Changed)
      {
        StoredSessions->Save();
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::CreateLogMemo()
{
  assert(!FLogMemo);
  assert(ActiveTerminal);
  FLogMemo = new TLogMemo(Application);
  try
  {
    FLogMemo->SessionLog = ActiveTerminal->Log;
    FLogMemo->PopupMenu = NonVisualDataModule->LogMemoPopup;
  }
  catch (...)
  {
    delete FLogMemo;
    throw;
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::FreeLogMemo()
{
  assert(LogMemo);
  LogMemo->PopupMenu = NULL;
  SAFE_DESTROY(FLogMemo);
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::ApplicationException(TObject * Sender, Exception * E)
{
  ShowExtendedException(E, Sender);
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::DeleteLocalFile(const AnsiString FileName)
{
  if (!RecursiveDeleteFile(FileName, WinConfiguration->DeleteToRecycleBin))
  {
    throw Exception(FMTLOAD(DELETE_LOCAL_FILE_ERROR, (FileName)));
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::TerminalQueryUser(TObject * /*Sender*/,
  const AnsiString Query, TStrings * MoreMessages, int Answers,
  int Params, int & Answer, TQueryType Type)
{
  AnsiString AQuery = Query;
  if (Params & qpFatalAbort)
  {
    AQuery = FMTLOAD(WARN_FATAL_ERROR, (AQuery));
  }

  int MessageParams = 0;
  if (Params & qpNeverAskAgainCheck)
  {
    MessageParams |= mpNeverAskAgainCheck;
  }
  if (Params & qpAllowContinueOnError)
  {
    MessageParams |= mpAllowContinueOnError;
  }

  if (ScpExplorer)
  {
    Answer = ScpExplorer->MoreMessageDialog(AQuery, MoreMessages, Type, Answers, 0, MessageParams);
  }
  else
  {
    Answer = MoreMessageDialog(AQuery, MoreMessages, Type, Answers, 0, MessageParams);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::OperationFinished(::TFileOperation Operation,
  TOperationSide Side, bool DragDrop, const AnsiString FileName, bool Success,
  bool & DisconnectWhenFinished)
{
  assert(ScpExplorer);
  ScpExplorer->OperationFinished(Operation, Side, DragDrop, FileName, Success,
    DisconnectWhenFinished);
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::OperationProgress(
  TFileOperationProgressType & ProgressData, TCancelStatus & Cancel)
{
  FProgress = ProgressData.InProgress ? ProgressData.OverallProgress() : -1;
  FOperation = ProgressData.Operation;              
  UpdateAppTitle();
  assert(ScpExplorer);
  ScpExplorer->OperationProgress(ProgressData, Cancel);
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::ConfigurationChange(TObject * /*Sender*/)
{
  assert(Configuration);
  assert(Configuration == WinConfiguration);

  if (!Application->Terminated && Configuration->Logging &&
      (WinConfiguration->LogView == lvWindow))
  {
    if (ActiveTerminal)
    {
      RequireLogForm(LogMemo);
    }
  }
    else
  {
    FreeLogForm();
  }

  if (ActiveTerminal)
  {
    assert(ActiveTerminal->Log);
    ActiveTerminal->Log->ReflectSettings();
  }

  if (ScpExplorer)
  {
    ScpExplorer->ConfigurationChanged();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::TerminalReady()
{
  ScpExplorer->Terminal = ActiveTerminal;
  if (OnChangeTerminal)
  {
    OnChangeTerminal(this);
  }
}
//---------------------------------------------------------------------------
TStrings * __fastcall TTerminalManager::GetTerminalList()
{
  if (FTerminalList->Count != Count)
  {
    for (int i = 0; i < Count; i++)
    {
      AnsiString NameN;
      AnsiString Name = Terminals[i]->SessionData->SessionName;
      int Number = 1;
      NameN = Name;
      while (FTerminalList->IndexOf(NameN) >= 0)
      {
        Number++;
        NameN = FORMAT("%s (%d)", (Name, Number));
      }
      if (Number > 1)
      {
        Name = FORMAT("%s (%d)", (Name, Number));
      }
      FTerminalList->AddObject(Name, Terminals[i]);
    }
  }
  return FTerminalList;
}
//---------------------------------------------------------------------------
int __fastcall TTerminalManager::GetActiveTerminalIndex()
{
  return ActiveTerminal ? IndexOf(ActiveTerminal) : -1;
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::SetActiveTerminalIndex(int value)
{
  ActiveTerminal = Terminals[value];
}
//---------------------------------------------------------------------------
AnsiString __fastcall TTerminalManager::GetActiveTerminalTitle()
{
  AnsiString Result = ActiveTerminal ?
    TerminalList->Strings[IndexOf(ActiveTerminal)] : AnsiString("");
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::CycleTerminals(bool Forward)
{
  int Index = ActiveTerminalIndex;
  Index += Forward ? 1 : -1;
  if (Index < 0)
  {
    Index = Count-1;
  }
  else if (Index >= Count)
  {
    Index = 0;
  }
  ActiveTerminalIndex = Index;
}

