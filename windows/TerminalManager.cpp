//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "TerminalManager.h"
#include <Authenticate.h>
#include "CustomScpExplorer.h"
#include "LogMemo.h"
#include "NonVisual.h"
#include "WinConfiguration.h"
#include <Log.h>
#include <Common.h>
#include <CoreMain.h>
#include <GUITools.h>
#include <TextsWin.h>
#include <Progress.h>
#include <Exceptions.h>
#include <VCLCommon.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
TTerminalManager * TTerminalManager::FInstance = NULL;
//---------------------------------------------------------------------------
__fastcall TManagedTerminal::TManagedTerminal(TSessionData * SessionData,
  TConfiguration * Configuration) :
  TTerminal(SessionData, Configuration),
  Color((TColor)SessionData->Color), SynchronizeBrowsing(false),
  LocalDirectory(SessionData->LocalDirectory),
  RemoteDirectory(SessionData->RemoteDirectory),
  LocalExplorerState(NULL), RemoteExplorerState(NULL)
{
}
//---------------------------------------------------------------------------
__fastcall TManagedTerminal::~TManagedTerminal()
{
  delete LocalExplorerState;
  delete RemoteExplorerState;
}
//---------------------------------------------------------------------------
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
  FQueueSection = new TCriticalSection();
  FLogMemo = NULL;
  FActiveTerminal = NULL;
  FScpExplorer = NULL;
  FDestroying = false;
  FTerminalPendingAction = tpNull;
  FDirectoryReadingStart = 0;
  FAuthenticateForm = NULL;
  FQueueWithEvent = NULL;

  assert(Application && !Application->OnException);
  Application->OnException = ApplicationException;
  assert(Application->OnShowHint == NULL);
  Application->OnShowHint = ApplicationShowHint;
  assert(Application->OnActivate == NULL);
  Application->OnActivate = ApplicationActivate;

  assert(Configuration && !Configuration->OnChange);
  Configuration->OnChange = ConfigurationChange;
  FOnLastTerminalClosed = NULL;
  FOnTerminalListChanged = NULL;

  FTerminalList = new TStringList();
  FQueues = new TList();
  FTerminationMessages = new TStringList();
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
  assert(Application->OnShowHint == ApplicationShowHint);
  Application->OnShowHint = ApplicationShowHint;
  assert(Application->OnActivate == ApplicationActivate);
  Application->OnActivate = NULL;

  delete FQueues;
  delete FTerminationMessages;
  delete FTerminalList;
  delete FAuthenticateForm;
  delete FQueueSection;
}
//---------------------------------------------------------------------------
TTerminalQueue * __fastcall TTerminalManager::NewQueue(TTerminal * Terminal)
{
  TTerminalQueue * Queue = new TTerminalQueue(Terminal, Configuration);
  Queue->TransfersLimit = GUIConfiguration->QueueTransfersLimit;
  Queue->OnQueryUser = TerminalQueryUser;
  Queue->OnPromptUser = TerminalPromptUser;
  Queue->OnShowExtendedException = TerminalShowExtendedException;
  Queue->OnEvent = QueueEvent;
  return Queue;
}
//---------------------------------------------------------------------------
TTerminal * __fastcall TTerminalManager::CreateTerminal(TSessionData * Data)
{
  return new TManagedTerminal(Data, Configuration);
}
//---------------------------------------------------------------------------
TTerminal * __fastcall TTerminalManager::NewTerminal(TSessionData * Data)
{
  FTerminalList->Clear();
  TTerminal * Terminal = TTerminalList::NewTerminal(Data);
  try
  {
    FQueues->Add(NewQueue(Terminal));
    FTerminationMessages->Add("");

    Terminal->OnQueryUser = TerminalQueryUser;
    Terminal->OnPromptUser = TerminalPromptUser;
    Terminal->OnDisplayBanner = TerminalDisplayBanner;
    Terminal->OnShowExtendedException = TerminalShowExtendedException;
    Terminal->OnProgress = OperationProgress;
    Terminal->OnFinished = OperationFinished;
    Terminal->OnDeleteLocalFile = DeleteLocalFile;
    Terminal->OnReadDirectoryProgress = TerminalReadDirectoryProgress;
    Terminal->OnInformation = TerminalInformation;

    if (!ActiveTerminal)
    {
      ActiveTerminal = Terminal;
    }
  }
  catch(...)
  {
    if (Terminal != NULL)
    {
      FreeTerminal(Terminal);
    }
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
    assert(FTerminalPendingAction == ::tpNone);
    FTerminalPendingAction = tpFree;
  }
}
//---------------------------------------------------------------------------
void TTerminalManager::ConnectTerminal(TTerminal * Terminal, bool Reopen)
{
  TManagedTerminal * ManagedTerminal = dynamic_cast<TManagedTerminal *>(Terminal);
  // it must be managed terminal, unless it is secondary terminal (of managed terminal)
  assert((ManagedTerminal != NULL) || (dynamic_cast<TSecondaryTerminal *>(Terminal) != NULL));

  // particularly when we are reconnecting RemoteDirectory of managed terminal
  // hold the last used remote directory as opposite to session data, which holds
  // the default remote directory.
  // make sure the last used directory is used, but the default is preserved too
  AnsiString OrigRemoteDirectory = Terminal->SessionData->RemoteDirectory;
  try
  {
    if (ManagedTerminal != NULL)
    {
      Terminal->SessionData->RemoteDirectory = ManagedTerminal->RemoteDirectory;
    }

    if (Reopen)
    {
      Terminal->Reopen(0);
    }
    else
    {
      Terminal->Open();
    }
  }
  __finally
  {
    Terminal->SessionData->RemoteDirectory = OrigRemoteDirectory;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminalManager::ConnectActiveTerminalImpl(bool Reopen)
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

      ConnectTerminal(ActiveTerminal, Reopen);

      if (ScpExplorer)
      {
        assert(ActiveTerminal->Status == ssOpened);
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
      FTerminalPendingAction = ::tpNone;
      try
      {
        assert(ActiveTerminal != NULL);
        ActiveTerminal->ShowExtendedException(&E);
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
bool __fastcall TTerminalManager::ConnectActiveTerminal()
{
  bool Result = ConnectActiveTerminalImpl(false);

  if (Result && WinConfiguration->AutoOpenInPutty && CanOpenInPutty())
  {
    try
    {
      OpenInPutty();
    }
    catch(Exception & E)
    {
      ShowExtendedExceptionEx(NULL, &E);
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::DisconnectActiveTerminal()
{
  assert(ActiveTerminal);

  int Index = IndexOf(ActiveTerminal);

  TTerminalQueue * OldQueue;
  TTerminalQueue * NewQueue;
  OldQueue = reinterpret_cast<TTerminalQueue *>(FQueues->Items[Index]);
  NewQueue = this->NewQueue(ActiveTerminal);
  FQueues->Items[Index] = NewQueue;
  ScpExplorer->Queue = NewQueue;
  delete OldQueue;
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::ReconnectActiveTerminal()
{
  assert(ActiveTerminal);

  if (ScpExplorer)
  {
    if (ScpExplorer->Terminal == ActiveTerminal)
    {
      ScpExplorer->UpdateTerminal(ActiveTerminal);
    }
  }

  try
  {
    if (FTerminalPendingAction == tpNull)
    {
      ConnectActiveTerminalImpl(true);
    }
    else
    {
      FTerminalPendingAction = tpReconnect;
    }
  }
  catch(...)
  {
    FreeActiveTerminal();
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
    if (Terminal->Active)
    {
      Terminal->Close();
    }
  }
  __finally
  {
    int Index = IndexOf(Terminal);
    FTerminalList->Clear();
    Extract(Terminal);

    TTerminalQueue * Queue;
    Queue = reinterpret_cast<TTerminalQueue *>(FQueues->Items[Index]);
    FQueues->Delete(Index);
    FTerminationMessages->Delete(Index);

    if (ActiveTerminal && (Terminal == ActiveTerminal))
    {
      if ((Count > 0) && !FDestroying)
      {
        for (int i = 0; i < Count; i++)
        {
          if (Terminals[i]->Status == ssOpened)
          {
            ActiveTerminal = Terminals[i];
            break;
          }
        }
        if (ActiveTerminal == Terminal)
        {
          ActiveTerminal = Terminals[Index < Count ? Index : 0];
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

    // only now all references to/from queue (particularly events to explorer)
    // are cleared
    delete Queue;
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
      FScpExplorer->Terminal = ActiveTerminal;
      FScpExplorer->Queue = ActiveQueue;
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
    // here used to be call to TCustomScpExporer::UpdateSessionData (now UpdateTerminal)
    // but it seems to be duplicate to call from TCustomScpExporer::TerminalChanging

    TTerminal * PActiveTerminal = ActiveTerminal;
    FActiveTerminal = NULL;
    FActiveTerminal = value;
    // moved from else block of next if (ActiveTerminal) statement
    // so ScpExplorer can update its caption
    UpdateAppTitle();
    if (ScpExplorer)
    {
      if (ActiveTerminal && (ActiveTerminal->Status == ssOpened))
      {
        TerminalReady();
      }
      else
      {
        ScpExplorer->Terminal = NULL;
        ScpExplorer->Queue = NULL;
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

      int Index = ActiveTerminalIndex;
      if (!ActiveTerminal->Active && !FTerminationMessages->Strings[Index].IsEmpty())
      {
        AnsiString Message = FTerminationMessages->Strings[Index];
        FTerminationMessages->Strings[Index] = "";
        Exception * E = new ESshFatal(NULL, Message);
        try
        {
          // finally show pending terminal message,
          // this gives user also possibility to reconnect
          ActiveTerminal->ShowExtendedException(E);
        }
        __finally
        {
          delete E;
        }
      }
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
AnsiString __fastcall TTerminalManager::UpdateAppTitle()
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

  if (!FProgressTitle.IsEmpty())
  {
    NewTitle = FProgressTitle + " - " + NewTitle;
  }
  else if (ActiveTerminal && (ScpExplorer != NULL))
  {
    AnsiString Path = ScpExplorer->PathForCaption();
    if (!Path.IsEmpty())
    {
      NewTitle = Path + " - " + NewTitle;
    }
  }

  Application->Title = NewTitle;

  if (ScpExplorer)
  {
    ScpExplorer->ApplicationTitleChanged();
  }

  return NewTitle;
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
        TManagedTerminal * ManagedTerminal = dynamic_cast<TManagedTerminal *>(Terminal);
        assert(ManagedTerminal != NULL);
        Data->LocalDirectory = ManagedTerminal->LocalDirectory;
        Data->RemoteDirectory = ManagedTerminal->RemoteDirectory;
        Changed = true;
      }

      if (Changed)
      {
        // modified only, implicit
        StoredSessions->Save(false, false);
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
void __fastcall TTerminalManager::ApplicationException(TObject * /*Sender*/,
  Exception * E)
{
  // can be null for example when exception is thrown on login dialog
  if (ActiveTerminal != NULL)
  {
    ActiveTerminal->ShowExtendedException(E);
  }
  else
  {
    ShowExtendedException(E);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::ApplicationShowHint(AnsiString & HintStr,
  bool & /*CanShow*/, THintInfo & HintInfo)
{
  TLabel * HintLabel = dynamic_cast<TLabel *>(HintInfo.HintControl);
  if ((HintLabel != NULL) && (HintLabel->Caption == HintStr))
  {
    // Hack for transfer setting labels.
    // Should be converted to something like HintLabel()
    HintInfo.HintPos = HintLabel->ClientToScreen(TPoint(0, 0));
    HintInfo.HintMaxWidth = HintLabel->Width;
    HintInfo.HideTimeout = 100000; // "almost" never
  }
  else if (dynamic_cast<TProgressBar *>(HintInfo.HintControl) != NULL)
  {
    HintInfo.HideTimeout = 100000; // "almost" never
    HintInfo.ReshowTimeout = 500; // updated each 0.5s
  }
  else
  {
    HintInfo.HintMaxWidth = 300;
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::ApplicationActivate(TObject * /*Sender*/)
{
  // make sure the taskbar button of main window is pressed
  // (but only if main window is our application focused window,
  // note that there can also be an editor window)
  if ((ScpExplorer != NULL) && (Screen->ActiveForm == ScpExplorer))
  {
    // unfortunatelly this causes the main window to redraw (flicker).
    // also the same problem happens for any top-level window (login, authentication),
    // not only to main explorer, so the solution should be generalized
    SetActiveWindow(Application->Handle);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::DeleteLocalFile(const AnsiString FileName, bool Alternative)
{
  if (!RecursiveDeleteFile(FileName, (WinConfiguration->DeleteToRecycleBin != Alternative)))
  {
    throw Exception(FMTLOAD(DELETE_LOCAL_FILE_ERROR, (FileName)));
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::TerminalQueryUser(TObject * Sender,
  const AnsiString Query, TStrings * MoreMessages, int Answers,
  const TQueryParams * Params, int & Answer, TQueryType Type, void * /*Arg*/)
{
  AnsiString HelpKeyword;
  TMessageParams MessageParams(Params);
  AnsiString AQuery = Query;

  if (Params != NULL)
  {
    HelpKeyword = Params->HelpKeyword;

    if (FLAGSET(Params->Params, qpFatalAbort))
    {
      AQuery = FMTLOAD(WARN_FATAL_ERROR, (AQuery));

      if (!MessageParams.TimerMessage.IsEmpty())
      {
        MessageParams.TimerMessage = FMTLOAD(WARN_FATAL_ERROR, (MessageParams.TimerMessage));
      }
    }
  }

  if (ScpExplorer)
  {
    Answer = ScpExplorer->MoreMessageDialog(AQuery, MoreMessages, Type, Answers,
      HelpKeyword, &MessageParams, dynamic_cast<TTerminal *>(Sender));
  }
  else
  {
    Answer = MoreMessageDialog(AQuery, MoreMessages, Type, Answers, HelpKeyword,
      &MessageParams);
  }
}
//---------------------------------------------------------------------------
TAuthenticateForm * __fastcall TTerminalManager::MakeAuthenticateForm(
  TSessionData * Data)
{
  TAuthenticateForm * Dialog = SafeFormCreate<TAuthenticateForm>();
  Dialog->Init(Data);
  return Dialog;
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::TerminalPromptUser(
  TTerminal * Terminal, TPromptKind Kind, AnsiString Name, AnsiString Instructions,
  TStrings * Prompts, TStrings * Results, bool & Result, void * /*Arg*/)
{
  if ((Kind == pkPrompt) && (FAuthenticateForm == NULL) &&
      (Terminal->Status != ssOpening))
  {
    assert(Instructions.IsEmpty());
    assert(Prompts->Count == 1);
    assert(bool(Prompts->Objects[0]));
    AnsiString AResult = Results->Strings[0];
    Result = InputDialog(Name, Prompts->Strings[0], AResult);
    if (Result)
    {
      Results->Strings[0] = AResult;
    }
  }
  else
  {
    TAuthenticateForm * AuthenticateForm = FAuthenticateForm;
    if (AuthenticateForm == NULL)
    {
      AuthenticateForm = MakeAuthenticateForm(Terminal->SessionData);
    }

    try
    {
      Result = AuthenticateForm->PromptUser(Kind, Name, Instructions, Prompts, Results,
        (FAuthenticateForm != NULL), Terminal->StoredCredentialsTried);
    }
    __finally
    {
      if (FAuthenticateForm == NULL)
      {
        delete AuthenticateForm;
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::TerminalDisplayBanner(
  TTerminal * Terminal, AnsiString SessionName,
  const AnsiString & Banner, bool & NeverShowAgain, int Options)
{
  assert(FAuthenticateForm != NULL);
  TAuthenticateForm * AuthenticateForm = FAuthenticateForm;
  if (AuthenticateForm == NULL)
  {
    AuthenticateForm = MakeAuthenticateForm(Terminal->SessionData);
  }

  try
  {
    AuthenticateForm->Banner(Banner, NeverShowAgain, Options);
  }
  __finally
  {
    if (FAuthenticateForm == NULL)
    {
      delete AuthenticateForm;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::TerminalShowExtendedException(
  TTerminal * Terminal, Exception * E, void * /*Arg*/)
{
  if (ScpExplorer)
  {
    ScpExplorer->ShowExtendedException(Terminal, E);
  }
  else
  {
    ShowExtendedExceptionEx(Terminal, E);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::TerminalReadDirectoryProgress(
  TObject * /*Sender*/, int Progress, bool & Cancel)
{
  static TDateTime DirectoryReadingProgressDelay(0, 0, 1, 500);

  if (Progress == 0)
  {
    if (ScpExplorer != NULL)
    {
      ScpExplorer->LockWindow();
    }
    FDirectoryReadingStart = Now();
    if (!FProgressTitle.IsEmpty())
    {
      FProgressTitle = "";
      UpdateAppTitle();
    }

    // Reset "was ESC ever pressed?" state
    GetAsyncKeyState(VK_ESCAPE);
  }
  else if (Progress < 0)
  {
    if (Progress == -2)
    {
      // cancelled
      if (ScpExplorer != NULL)
      {
        ScpExplorer->ReadDirectoryCancelled();
      }
    }
    else
    {
      if (ScpExplorer != NULL)
      {
        ScpExplorer->UnlockWindow();
      }

      FProgressTitle = "";
      UpdateAppTitle();
    }
  }
  else
  {
    // If the least significant bit is set,
    // the key was pressed after the previous call to GetAsyncKeyState.
    int KeyState = GetAsyncKeyState(VK_ESCAPE);
    if (FLAGSET(KeyState, 0x01))
    {
      Cancel = true;
    }

    if ((Now() - FDirectoryReadingStart) >= DirectoryReadingProgressDelay)
    {
      FProgressTitle = FMTLOAD(DIRECTORY_READING_PROGRESS, (Progress));
      UpdateAppTitle();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::TerminalInformation(
  TTerminal * Terminal, const AnsiString & Str, bool /*Status*/, bool Active)
{
  if (Active)
  {
    if (Terminal->Status == ssOpening)
    {
      bool ShowPending = false;
      if (FAuthenticateForm == NULL)
      {
        FAuthenticateForm = MakeAuthenticateForm(Terminal->SessionData);
        ShowPending = true;
        Busy(true);
      }
      FAuthenticateForm->Log(Str);
      if (ShowPending)
      {
        FAuthenticateForm->ShowAsModal();
      }
    }
  }
  else
  {
    Busy(false);
    SAFE_DESTROY(FAuthenticateForm);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::OperationFinished(::TFileOperation Operation,
  TOperationSide Side, bool Temp, const AnsiString FileName, bool Success,
  bool & DisconnectWhenFinished)
{
  assert(ScpExplorer);
  ScpExplorer->OperationFinished(Operation, Side, Temp, FileName, Success,
    DisconnectWhenFinished);
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::OperationProgress(
  TFileOperationProgressType & ProgressData, TCancelStatus & Cancel)
{
  if (ProgressData.InProgress)
  {
    FProgressTitle = FORMAT("%d%% %s",
      (ProgressData.OverallProgress(),
       TProgressForm::OperationName(ProgressData.Operation)));
  }
  else
  {
    FProgressTitle = "";
  }

  UpdateAppTitle();
  assert(ScpExplorer);
  ScpExplorer->OperationProgress(ProgressData, Cancel);
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::QueueEvent(TTerminalQueue * Queue, TQueueEvent Event)
{
  TGuard Guard(FQueueSection);
  FQueueWithEvent = Queue;
  FQueueEvent = Event;
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

  TTerminalQueue * Queue;
  for (int Index = 0; Index < Count; Index++)
  {
    assert(Terminals[Index]->Log);
    Terminals[Index]->Log->ReflectSettings();
    Queue = reinterpret_cast<TTerminalQueue *>(FQueues->Items[Index]);
    Queue->TransfersLimit = GUIConfiguration->QueueTransfersLimit;
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
  ScpExplorer->Queue = ActiveQueue;
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
AnsiString __fastcall TTerminalManager::TerminalTitle(TTerminal * Terminal)
{
  int Index = IndexOf(Terminal);
  assert(Index >= 0);
  AnsiString Result;
  if (Index >= 0)
  {
    Result = TerminalList->Strings[Index];
  }
  else
  {
    Result = Terminal->SessionData->SessionName;
  }
  return Result;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TTerminalManager::GetActiveTerminalTitle()
{
  AnsiString Result = ActiveTerminal ?
    TerminalTitle(ActiveTerminal) : AnsiString("");
  return Result;
}
//---------------------------------------------------------------------------
TTerminalQueue * __fastcall TTerminalManager::GetActiveQueue()
{
  TTerminalQueue * Result = NULL;
  if (ActiveTerminal != NULL)
  {
    Result = reinterpret_cast<TTerminalQueue *>(FQueues->Items[ActiveTerminalIndex]);
  }
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
//---------------------------------------------------------------------------
bool __fastcall TTerminalManager::CanOpenInPutty()
{
  return (ActiveTerminal != NULL) && !GUIConfiguration->PuttyPath.Trim().IsEmpty();
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::OpenInPutty()
{
  TSessionData * Data = new TSessionData("");
  try
  {
    Data->Assign(ActiveTerminal->SessionData);
    if (ActiveTerminal->TunnelLocalPortNumber != 0)
    {
      Data->ConfigureTunnel(ActiveTerminal->TunnelLocalPortNumber);
    }

    assert(ActiveTerminal != NULL);
    OpenSessionInPutty(GUIConfiguration->PuttyPath, Data,
      GUIConfiguration->PuttyPassword ? ActiveTerminal->Password : AnsiString());
  }
  __finally
  {
    delete Data;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminalManager::NewSession()
{
  bool Result;
  TSessionData * Data = new TSessionData("");
  try
  {
    Data->Assign(StoredSessions->DefaultSettings);
    if (DoLoginDialog(StoredSessions, Data, loAddSession))
    {
      if ((Data->FSProtocol == fsExternalSSH) ||
          (Data->FSProtocol == fsExternalSFTP))
      {
        OpenSessionInPutty(
          ((Data->FSProtocol == fsExternalSSH) ?
            GUIConfiguration->PuttyPath : GUIConfiguration->PSftpPath),
          Data, (GUIConfiguration->PuttyPassword ? Data->Password : AnsiString()));
        Result = false;
      }
      else
      {
        assert(Data->CanLogin);
        TTerminalManager * Manager = TTerminalManager::Instance();
        TTerminal * Terminal = Manager->NewTerminal(Data);
        Manager->ActiveTerminal = Terminal;
        Result = Manager->ConnectActiveTerminal();
      }
    }
  }
  __finally
  {
    delete Data;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::Idle()
{
  for (int Index = 0; Index < Count; Index++)
  {
    TTerminal * Terminal = Terminals[Index];
    try
    {
      if (Terminal->Active)
      {
        Terminal->Idle();

        assert(Index < FQueues->Count);
        if (Index < FQueues->Count)
        {
          reinterpret_cast<TTerminalQueue *>(FQueues->Items[Index])->Idle();
        }
      }
    }
    catch(Exception & E)
    {
      if (Terminal == ActiveTerminal)
      {
        Terminal->ShowExtendedException(&E);
      }
      else
      {
        // we may not have inactive terminal, unless there is a explorer,
        // also Idle is calls frome explorer anyway
        assert(ScpExplorer != NULL);
        if (ScpExplorer != NULL)
        {
          ScpExplorer->InactiveTerminalException(Terminal, &E);
        }

        if (!Terminal->Active)
        {
          // if session is lost, save the error message and rethrow it
          // once the terminal gets activated
          FTerminationMessages->Strings[Index] = E.Message;
        }
      }
    }
  }

  TTerminalQueue * QueueWithEvent;
  TQueueEvent QueueEvent;
  {
    TGuard Guard(FQueueSection);

    QueueWithEvent = FQueueWithEvent;
    FQueueWithEvent = NULL;
    QueueEvent = FQueueEvent;
  }

  if (QueueWithEvent != NULL)
  {
    int Index = FQueues->IndexOf(QueueWithEvent);
    // the session may not exist anymore
    if (Index >= 0)
    {
      TTerminal * Terminal = Terminals[Index];
      // we can hardly have a queue event without explorer
      assert(ScpExplorer != NULL);
      if (ScpExplorer != NULL)
      {
        ScpExplorer->QueueEvent(Terminal, QueueWithEvent, QueueEvent);
      }
    }
  }
}
