//---------------------------------------------------------------------------
#define NO_WIN32_LEAN_AND_MEAN
#include <vcl.h>
#pragma hdrstop

#include "TerminalManager.h"
#include <Authenticate.h>
#include "CustomScpExplorer.h"
#include "LogMemo.h"
#include "NonVisual.h"
#include "WinConfiguration.h"
#include "Tools.h"
#include <Log.h>
#include <Common.h>
#include <CoreMain.h>
#include <GUITools.h>
#include <TextsWin.h>
#include <TextsCore.h>
#include <Progress.h>
#include <Exceptions.h>
#include <VCLCommon.h>
#include <WinApi.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
TTerminalManager * TTerminalManager::FInstance = NULL;
//---------------------------------------------------------------------------
__fastcall TManagedTerminal::TManagedTerminal(TSessionData * SessionData,
  TConfiguration * Configuration) :
  TTerminal(SessionData, Configuration),
  LocalExplorerState(NULL), RemoteExplorerState(NULL),
  ReopenStart(0), DirectoryLoaded(Now()), TerminalThread(NULL)
{
  StateData = new TSessionData(L"");
  StateData->Assign(SessionData);
  StateData->LocalDirectory = ::ExpandFileName(StateData->LocalDirectory);
}
//---------------------------------------------------------------------------
__fastcall TManagedTerminal::~TManagedTerminal()
{
  delete StateData;
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
  FTaskbarList = NULL;
  FAuthenticating = 0;

  assert(Application && !Application->OnException);
  Application->OnException = ApplicationException;
  assert(Application->OnShowHint == NULL);
  Application->OnShowHint = ApplicationShowHint;
  assert(Application->OnMessage == NULL);
  Application->OnMessage = ApplicationMessage;
  assert(Application->OnModalBegin == NULL);
  Application->OnModalBegin = ApplicationModalBegin;
  assert(Application->OnModalEnd == NULL);
  Application->OnModalEnd = ApplicationModalEnd;
  assert(WinConfiguration->OnMasterPasswordPrompt == NULL);
  WinConfiguration->OnMasterPasswordPrompt = MasterPasswordPrompt;

  InitTaskbarButtonCreatedMessage();

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
  assert(Application->OnMessage == ApplicationMessage);
  Application->OnMessage = NULL;
  assert(Application->OnModalBegin == ApplicationModalBegin);
  Application->OnModalBegin = NULL;
  assert(Application->OnModalEnd == ApplicationModalEnd);
  Application->OnModalEnd = NULL;
  assert(WinConfiguration->OnMasterPasswordPrompt == MasterPasswordPrompt);
  WinConfiguration->OnMasterPasswordPrompt = NULL;

  delete FQueues;
  delete FTerminationMessages;
  delete FTerminalList;
  delete FAuthenticateForm;
  delete FQueueSection;
  ReleaseTaskbarList();
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::SetQueueConfiguration(TTerminalQueue * Queue)
{
  Queue->TransfersLimit = GUIConfiguration->QueueTransfersLimit;
  Queue->KeepDoneItemsFor =
    (GUIConfiguration->QueueKeepDoneItems ? GUIConfiguration->QueueKeepDoneItemsFor : 0);
}
//---------------------------------------------------------------------------
TTerminalQueue * __fastcall TTerminalManager::NewQueue(TTerminal * Terminal)
{
  TTerminalQueue * Queue = new TTerminalQueue(Terminal, Configuration);
  SetQueueConfiguration(Queue);
  Queue->Enabled = WinConfiguration->EnableQueueByDefault;
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
TTerminal * __fastcall TTerminalManager::DoNewTerminal(TSessionData * Data)
{
  FTerminalList->Clear();
  TTerminal * Terminal = TTerminalList::NewTerminal(Data);
  try
  {
    FQueues->Add(NewQueue(Terminal));
    FTerminationMessages->Add(L"");

    Terminal->OnQueryUser = TerminalQueryUser;
    Terminal->OnPromptUser = TerminalPromptUser;
    Terminal->OnDisplayBanner = TerminalDisplayBanner;
    Terminal->OnShowExtendedException = TerminalShowExtendedException;
    Terminal->OnProgress = OperationProgress;
    Terminal->OnFinished = OperationFinished;
    Terminal->OnDeleteLocalFile = DeleteLocalFile;
    Terminal->OnReadDirectoryProgress = TerminalReadDirectoryProgress;
    Terminal->OnInformation = TerminalInformation;
  }
  catch(...)
  {
    if (Terminal != NULL)
    {
      FreeTerminal(Terminal);
    }
    throw;
  }

  return Terminal;
}
//---------------------------------------------------------------------------
TTerminal * __fastcall TTerminalManager::NewTerminal(TSessionData * Data)
{
  TTerminal * Terminal = DoNewTerminal(Data);
  DoTerminalListChanged();
  return Terminal;
}
//---------------------------------------------------------------------------
TTerminal * __fastcall TTerminalManager::NewTerminals(TList * DataList)
{
  TTerminal * Result = NULL;
  for (int Index = 0; Index < DataList->Count; Index++)
  {
    TTerminal * Terminal =
      NewTerminal(reinterpret_cast<TSessionData *>(DataList->Items[Index]));
    if (Index == 0)
    {
      Result = Terminal;
    }
  }
  DoTerminalListChanged();
  return Result;
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
  UnicodeString OrigRemoteDirectory = Terminal->SessionData->RemoteDirectory;
  try
  {
    TTerminalThread * TerminalThread = new TTerminalThread(Terminal);
    try
    {
      if (ManagedTerminal != NULL)
      {
        Terminal->SessionData->RemoteDirectory = ManagedTerminal->StateData->RemoteDirectory;

        if ((double)ManagedTerminal->ReopenStart == 0)
        {
          ManagedTerminal->ReopenStart = Now();
        }

        assert(ManagedTerminal->TerminalThread == NULL);
        ManagedTerminal->TerminalThread = TerminalThread;
      }

      TNotifyEvent OnIdle;
      ((TMethod*)&OnIdle)->Code = TerminalThreadIdle;
      TerminalThread->OnIdle = OnIdle;
      if (Reopen)
      {
        TerminalThread->TerminalReopen();
      }
      else
      {
        TerminalThread->TerminalOpen();
      }
    }
    __finally
    {
      if (ManagedTerminal != NULL)
      {
        ManagedTerminal->TerminalThread = NULL;
      }

      delete TerminalThread;
    }
  }
  __finally
  {
    Terminal->SessionData->RemoteDirectory = OrigRemoteDirectory;
    if (Terminal->Active && (ManagedTerminal != NULL))
    {
      ManagedTerminal->ReopenStart = 0;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::TerminalThreadIdle(void * /*Data*/, TObject * /*Sender*/)
{
  Application->ProcessMessages();
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
  ActiveTerminal->CollectUsage();

  // add only stored sessions to the jump list,
  // ad-hoc session cannot be reproduced from just a session name
  if (StoredSessions->FindSame(ActiveTerminal->SessionData) != NULL)
  {
    WinConfiguration->AddSessionToJumpList(ActiveTerminal->SessionData->SessionName);
  }

  FAuthenticationCancelled = false;
  bool Result = ConnectActiveTerminalImpl(false);

  UnicodeString DateStamp = StandardDatestamp();
  if (Result)
  {
    if (Configuration->Usage->Get(L"OpenedSessionsFailedLastDate") == DateStamp)
    {
      Configuration->Usage->Inc(L"OpenedSessionsFailedRecovered");
    }
  }
  else
  {
    Configuration->Usage->Inc(L"OpenedSessionsFailed");
    Configuration->Usage->Set(L"OpenedSessionsFailedLastDate", DateStamp);
    if (FAuthenticationCancelled)
    {
      Configuration->Usage->Inc(L"OpenedSessionsFailedAfterCancel");
    }
  }

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
    // we want the Login dialog to open on auto-workspace name,
    // as set in TCustomScpExplorerForm::FormClose
    if (!FDestroying || !WinConfiguration->AutoSaveWorkspace)
    {
      if (StoredSessions->FindSame(Terminal->SessionData) != NULL)
      {
        WinConfiguration->LastStoredSession = Terminal->SessionData->Name;
      }
    }

    if (ScpExplorer != NULL)
    {
      ScpExplorer->TerminalRemoved(Terminal);
    }

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
        ActiveTerminal = Terminals[Index < Count ? Index : Index - 1];
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

    DoTerminalListChanged();
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
      UpdateTaskbarList();
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
  DoSetActiveTerminal(value, false);
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::SetActiveTerminalWithAutoReconnect(TTerminal * value)
{
  DoSetActiveTerminal(value, true);
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::DoSetActiveTerminal(TTerminal * value, bool AutoReconnect)
{
  if (ActiveTerminal != value)
  {
    // here used to be call to TCustomScpExporer::UpdateSessionData (now UpdateTerminal)
    // but it seems to be duplicate to call from TCustomScpExporer::TerminalChanging

    TTerminal * PActiveTerminal = ActiveTerminal;
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
        UnicodeString Message = FTerminationMessages->Strings[Index];
        FTerminationMessages->Strings[Index] = L"";
        if (AutoReconnect)
        {
          ReconnectActiveTerminal();
        }
        else
        {
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

    if ((ActiveTerminal != NULL) && !ActiveTerminal->Active)
    {
      ConnectActiveTerminal();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::QueueStatusUpdated()
{
  UpdateAppTitle();
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::UpdateAppTitle()
{
  if (ScpExplorer)
  {
    UnicodeString NewTitle;
    if (ActiveTerminal)
    {
      NewTitle = FormatMainFormCaption(ActiveTerminalTitle);
    }
    else
    {
      NewTitle = FormatMainFormCaption(L"");
    }

    UnicodeString QueueProgressTitle;
    if (!FForegroundProgressTitle.IsEmpty())
    {
      NewTitle = FForegroundProgressTitle + L" - " + NewTitle;
    }
    else if (!FProgressTitle.IsEmpty() && !ForegroundTask())
    {
      NewTitle = FProgressTitle + L" - " + NewTitle;
    }
    else if ((ScpExplorer != NULL) && (ScpExplorer->Handle != GetAncestor(GetActiveWindow(), GA_ROOTOWNER)) &&
             !(QueueProgressTitle = ScpExplorer->GetQueueProgressTitle()).IsEmpty())
    {
      NewTitle = QueueProgressTitle + L" - " + NewTitle;
    }
    else if (ActiveTerminal && (ScpExplorer != NULL))
    {
      UnicodeString Path = ScpExplorer->PathForCaption();
      if (!Path.IsEmpty())
      {
        NewTitle = Path + L" - " + NewTitle;
      }
    }

    ScpExplorer->Caption = NewTitle;
    ScpExplorer->ApplicationTitleChanged();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::SaveTerminal(TTerminal * Terminal)
{
  TSessionData * Data = StoredSessions->FindSame(Terminal->SessionData);
  if (Data != NULL)
  {
    TManagedTerminal * ManagedTerminal = dynamic_cast<TManagedTerminal *>(Terminal);
    assert(ManagedTerminal != NULL);

    bool Changed = false;
    if (Terminal->SessionData->UpdateDirectories)
    {
      Data->CopyDirectoriesStateData(ManagedTerminal->StateData);
      Changed = true;
    }

    if (Changed)
    {
      // modified only, implicit
      StoredSessions->Save(false, false);
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
void __fastcall TTerminalManager::HandleException(Exception * E)
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
void __fastcall TTerminalManager::ApplicationException(TObject * /*Sender*/,
  Exception * E)
{
  HandleException(E);
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::ApplicationShowHint(UnicodeString & HintStr,
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
    // Hint is forcibly hidden in TProgressForm::FormHide
    HintInfo.HideTimeout = 100000; // "almost" never
    HintInfo.ReshowTimeout = 500; // updated each 0.5s
  }
  else
  {
    HintInfo.HintMaxWidth = 300;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminalManager::HandleMouseWheel(WPARAM WParam, LPARAM LParam)
{
  bool Result = false;
  if (Application->Active)
  {
    TPoint Point(LOWORD(LParam), HIWORD(LParam));
    TWinControl * Control = FindVCLWindow(Point);
    if (Control != NULL)
    {
      TCustomForm * Form = ValidParentForm(Control);
      if (Form->Active)
      {
        // Send it only to windows we tested it with.
        // Though we should sooner or later remote this test and pass it to all our windows.
        if (Form->Perform(WM_WANTS_MOUSEWHEEL, 0, 0) == 1)
        {
          SendMessage(Control->Handle, WM_MOUSEWHEEL, WParam, LParam);
          Result = true;
        }
      }
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::ApplicationMessage(TMsg & Msg, bool & Handled)
{
  if (Msg.message == FTaskbarButtonCreatedMessage)
  {
    CreateTaskbarList();
  }
  if (Msg.message == WM_MOUSEWHEEL)
  {
    Handled = HandleMouseWheel(Msg.wParam, Msg.lParam);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::ApplicationModalBegin(TObject * /*Sender*/)
{
  NonVisualDataModule->StartBusy();
  if (ScpExplorer != NULL)
  {
    ScpExplorer->SuspendWindowLock();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::ApplicationModalEnd(TObject * /*Sender*/)
{
  NonVisualDataModule->EndBusy();
  if (ScpExplorer != NULL)
  {
    ScpExplorer->ResumeWindowLock();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::InitTaskbarButtonCreatedMessage()
{
  // XE6 VCL already handles TaskbarButtonCreated, but does not call ChangeWindowMessageFilterEx.
  // So we keep our implementation.
  // See also http://stackoverflow.com/a/14618587/850848
  FTaskbarButtonCreatedMessage = RegisterWindowMessage(L"TaskbarButtonCreated");

  HINSTANCE User32Library = LoadLibrary(L"user32.dll");
  ChangeWindowMessageFilterExProc ChangeWindowMessageFilterEx =
    (ChangeWindowMessageFilterExProc)GetProcAddress(User32Library, "ChangeWindowMessageFilterEx");

  if (ChangeWindowMessageFilterEx != NULL)
  {
    // without this we won't get TaskbarButtonCreated, when app is running elevated
    ChangeWindowMessageFilterEx(
      Application->Handle, FTaskbarButtonCreatedMessage, MSGFLT_ALLOW, NULL);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::CreateTaskbarList()
{

  ReleaseTaskbarList();

  if(SUCCEEDED(CoCreateInstance(CLSID_TaskbarList, NULL, CLSCTX_ALL,
        IID_ITaskbarList3, (void **) &FTaskbarList)))
  {
    if (ScpExplorer != NULL)
    {
      UpdateTaskbarList();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::ReleaseTaskbarList()
{
  if (FTaskbarList != NULL)
  {
    FTaskbarList->Release();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::UpdateTaskbarList()
{
  ScpExplorer->UpdateTaskbarList(FTaskbarList);
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::DeleteLocalFile(const UnicodeString FileName, bool Alternative)
{
  if (!RecursiveDeleteFile(FileName, (WinConfiguration->DeleteToRecycleBin != Alternative)))
  {
    throw EOSExtException(FMTLOAD(DELETE_LOCAL_FILE_ERROR, (FileName)));
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::TerminalQueryUser(TObject * Sender,
  const UnicodeString Query, TStrings * MoreMessages, unsigned int Answers,
  const TQueryParams * Params, unsigned int & Answer, TQueryType Type, void * /*Arg*/)
{
  UnicodeString HelpKeyword;
  TMessageParams MessageParams(Params);
  UnicodeString AQuery = Query;

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
void __fastcall TTerminalManager::AuthenticateFormCancel(TObject * Sender)
{
  TAuthenticateForm * Form = dynamic_cast<TAuthenticateForm *>(Sender);
  assert(Form != NULL);
  TManagedTerminal * ManagedTerminal = dynamic_cast<TManagedTerminal *>(Form->Terminal);
  // will be null e.g. for background transfers
  if (ManagedTerminal != NULL)
  {
    TTerminalThread * TerminalThread = ManagedTerminal->TerminalThread;
    // can be NULL for reconnects from transfers
    if ((TerminalThread != NULL) && !TerminalThread->Cancelling)
    {
      Form->Log(LoadStr(AUTH_CANCELLING));
      TerminalThread->Cancel();
    }
  }
  FAuthenticationCancelled = true;
}
//---------------------------------------------------------------------------
TAuthenticateForm * __fastcall TTerminalManager::MakeAuthenticateForm(
  TTerminal * Terminal)
{
  TAuthenticateForm * Dialog = SafeFormCreate<TAuthenticateForm>();
  Dialog->Init(Terminal);
  assert(Dialog->OnCancel == NULL);
  Dialog->OnCancel = AuthenticateFormCancel;
  return Dialog;
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::FileNameInputDialogInitializeRenameBaseName(
  TObject * /*Sender*/, TInputDialogData * Data)
{
  EditSelectBaseName(Data->Edit->Handle);
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::TerminalPromptUser(
  TTerminal * Terminal, TPromptKind Kind, UnicodeString Name, UnicodeString Instructions,
  TStrings * Prompts, TStrings * Results, bool & Result, void * /*Arg*/)
{
  if (((Kind == pkPrompt) || (Kind == pkFileName)) && (FAuthenticateForm == NULL) &&
      (Terminal->Status != ssOpening))
  {
    assert(Instructions.IsEmpty());
    assert(Prompts->Count == 1);
    assert(FLAGSET(int(Prompts->Objects[0]), pupEcho));
    UnicodeString AResult = Results->Strings[0];

    TInputDialogInitialize InputDialogInitialize = NULL;
    if ((Kind == pkFileName) && !WinConfiguration->RenameWholeName)
    {
      InputDialogInitialize = FileNameInputDialogInitializeRenameBaseName;
    }

    Result = InputDialog(Name, Prompts->Strings[0], AResult, L"", NULL, false, InputDialogInitialize);
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
      AuthenticateForm = MakeAuthenticateForm(Terminal);
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
  TTerminal * Terminal, UnicodeString SessionName,
  const UnicodeString & Banner, bool & NeverShowAgain, int Options)
{
  assert(FAuthenticateForm != NULL);
  TAuthenticateForm * AuthenticateForm = FAuthenticateForm;
  if (AuthenticateForm == NULL)
  {
    AuthenticateForm = MakeAuthenticateForm(Terminal);
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
static TDateTime DirectoryReadingProgressDelay(0, 0, 1, 500);
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::TerminalReadDirectoryProgress(
  TObject * /*Sender*/, int Progress, int ResolvedLinks, bool & Cancel)
{
  if (Progress == 0)
  {
    if (ScpExplorer != NULL)
    {
      ScpExplorer->LockWindow();
    }
    FDirectoryReadingStart = Now();
    if (!FProgressTitle.IsEmpty() || !FForegroundProgressTitle.IsEmpty())
    {
      FProgressTitle = L"";
      FForegroundProgressTitle = L"";
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

      FProgressTitle = L"";
      FForegroundProgressTitle = L"";
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
      // 4 is arbitrary number
      FForegroundProgressTitle =
        FMTLOAD(ResolvedLinks >= 4 ? DIRECTORY_READING_AND_RESOLVING_PROGRESS : DIRECTORY_READING_PROGRESS,
          (Progress));
      UpdateAppTitle();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::TerminalInformation(
  TTerminal * Terminal, const UnicodeString & Str, bool /*Status*/, int Phase)
{

  if (Phase == 1)
  {
    if (FAuthenticating == 0)
    {
      FBusyToken = BusyStart();
    }
    FAuthenticating++;
  }
  else if (Phase == 0)
  {
    assert(FAuthenticating > 0);
    FAuthenticating--;
    if (FAuthenticating == 0)
    {
      BusyEnd(FBusyToken);
      FBusyToken = NULL;
    }
    SAFE_DESTROY(FAuthenticateForm);
  }
  else
  {
    if (FAuthenticating > 0)
    {
      bool ShowPending = false;
      if (FAuthenticateForm == NULL)
      {
        FAuthenticateForm = MakeAuthenticateForm(Terminal);
        ShowPending = true;
      }
      FAuthenticateForm->Log(Str);
      if (ShowPending)
      {
        FAuthenticateForm->ShowAsModal();
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::OperationFinished(::TFileOperation Operation,
  TOperationSide Side, bool Temp, const UnicodeString & FileName, bool Success,
  TOnceDoneOperation & OnceDoneOperation)
{
  assert(ScpExplorer);
  ScpExplorer->OperationFinished(Operation, Side, Temp, FileName, Success,
    OnceDoneOperation);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TTerminalManager::ProgressTitle(TFileOperationProgressType * ProgressData)
{
  return
    FORMAT(L"%d%% %s",
      (ProgressData->OverallProgress(),
       TProgressForm::OperationName(ProgressData->Operation, ProgressData->Side)));
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::OperationProgress(
  TFileOperationProgressType & ProgressData)
{
  if (ProgressData.InProgress)
  {
    FProgressTitle = ProgressTitle(&ProgressData);
  }
  else
  {
    FProgressTitle = L"";
  }

  UpdateAppTitle();
  assert(ScpExplorer);
  ScpExplorer->OperationProgress(ProgressData);
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::QueueEvent(TTerminalQueue * Queue, TQueueEvent Event)
{
  TGuard Guard(FQueueSection);
  FQueueEvents.push_back(std::make_pair(Queue, Event));
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
    Terminals[Index]->ActionLog->ReflectSettings();
    Queue = reinterpret_cast<TTerminalQueue *>(FQueues->Items[Index]);
    SetQueueConfiguration(Queue);
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
  ScpExplorer->TerminalReady();
}
//---------------------------------------------------------------------------
TStrings * __fastcall TTerminalManager::GetTerminalList()
{
  if (FTerminalList->Count != Count)
  {
    for (int i = 0; i < Count; i++)
    {
      UnicodeString NameN;
      UnicodeString Name = Terminals[i]->SessionData->SessionName;
      int Number = 1;
      NameN = Name;
      while (FTerminalList->IndexOf(NameN) >= 0)
      {
        Number++;
        NameN = FORMAT(L"%s (%d)", (Name, Number));
      }
      if (Number > 1)
      {
        Name = FORMAT(L"%s (%d)", (Name, Number));
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
UnicodeString __fastcall TTerminalManager::TerminalTitle(TTerminal * Terminal)
{
  int Index = IndexOf(Terminal);
  UnicodeString Result;
  if (Index >= 0)
  {
    Result = TerminalList->Strings[Index];
  }
  else
  {
    // this is the case of background transfer sessions
    Result = Terminal->SessionData->SessionName;
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TTerminalManager::GetActiveTerminalTitle()
{
  UnicodeString Result = ActiveTerminal ?
    TerminalTitle(ActiveTerminal) : UnicodeString(L"");
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
void __fastcall TTerminalManager::UpdateSessionCredentials(TSessionData * Data)
{
  Data->UserName = ActiveTerminal->UserName;
  Data->Password = ActiveTerminal->Password;
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::OpenInPutty()
{
  Configuration->Usage->Inc(L"OpenInPutty");

  TSessionData * Data = NULL;
  try
  {
    // Is NULL on the first session when called from ConnectActiveTerminal()
    // due to WinConfiguration->AutoOpenInPutty
    if (ScpExplorer != NULL)
    {
      Data = ScpExplorer->CloneCurrentSessionData();
    }
    else
    {
      Data = new TSessionData(L"");
      assert(ActiveTerminal != NULL);
      Data->Assign(ActiveTerminal->SessionData);
      UpdateSessionCredentials(Data);
    }

    // putty does not support resolving environment variables in session settings
    Data->ExpandEnvironmentVariables();

    if (ActiveTerminal->TunnelLocalPortNumber != 0)
    {
      Data->ConfigureTunnel(ActiveTerminal->TunnelLocalPortNumber);
    }

    OpenSessionInPutty(GUIConfiguration->PuttyPath, Data);
  }
  __finally
  {
    delete Data;
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::NewSession(bool /*FromSite*/, const UnicodeString & SessionUrl)
{
  UnicodeString DownloadFile; // unused
  std::unique_ptr<TObjectList> DataList(new TObjectList());

  GetLoginData(SessionUrl, NULL, DataList.get(), DownloadFile);

  if (DataList->Count > 0)
  {
    ActiveTerminal = NewTerminals(DataList.get());
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::Idle()
{
  for (int Index = 0; Index < Count; Index++)
  {
    TTerminal * Terminal = Terminals[Index];
    try
    {
      TManagedTerminal * ManagedTerminal = dynamic_cast<TManagedTerminal *>(Terminal);
      assert(ManagedTerminal != NULL);
      // make sure Idle is called on the thread that runs the terminal
      if (ManagedTerminal->TerminalThread != NULL)
      {
        ManagedTerminal->TerminalThread->Idle();
      }
      else
      {
        if (Terminal->Active)
        {
          Terminal->Idle();
        }
      }

      if (Terminal->Active)
      {
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
        // throw further, so that the exception is handled in proper place
        // (particularly in broken-transfer reconnect handler, bug 72)
        throw;
      }
      else
      {
        // we may not have inactive terminal, unless there is a explorer,
        // also Idle is called from explorer anyway
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

  do
  {
    QueueWithEvent = NULL;

    {
      TGuard Guard(FQueueSection);

      if (!FQueueEvents.empty())
      {
        QueueWithEvent = FQueueEvents[0].first;
        QueueEvent = FQueueEvents[0].second;
        FQueueEvents.erase(FQueueEvents.begin());
      }
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
  while (QueueWithEvent != NULL);
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::MasterPasswordPrompt()
{
  if (GetCurrentThreadId() == MainThreadID)
  {
    if (!DoMasterPasswordDialog())
    {
      Abort();
    }
  }
  else
  {
    // this can happen only when we keep cancelling all master password prompts
    // as long as the sessing finally connects (session password has to be
    // explictly typed in), and background transfer is started
    Abort();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::Move(TTerminal * Source, TTerminal * Target)
{
  FTerminalList->Clear();
  int SourceIndex = IndexOf(Source);
  int TargetIndex = IndexOf(Target);
  TTerminalList::Move(SourceIndex, TargetIndex);
  FQueues->Move(SourceIndex, TargetIndex);
  DoTerminalListChanged();
  // when there are indexed sessions with the same name,
  // the index may change when reordering the sessions
  UpdateAppTitle();
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::DoTerminalListChanged()
{
  if (OnTerminalListChanged)
  {
    OnTerminalListChanged(this);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalManager::SaveWorkspace(TList * DataList)
{
  for (int Index = 0; Index < Count; Index++)
  {
    TManagedTerminal * ManagedTerminal = dynamic_cast<TManagedTerminal *>(Terminals[Index]);
    TSessionData * Data = StoredSessions->SaveWorkspaceData(ManagedTerminal->StateData);
    DataList->Add(Data);
    Data->Name = IntToHex(Index, 4);
  }
}
//---------------------------------------------------------------------------
TTerminal * __fastcall TTerminalManager::FindActiveTerminalForSite(TSessionData * Data)
{
  TTerminal * Result = NULL;
  for (int Index = 0; (Result == NULL) && (Index < Count); Index++)
  {
    TTerminal * Terminal = Terminals[Index];
    if (Terminal->Active &&
        Terminal->SessionData->IsSameSite(Data))
    {
      Result = Terminal;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
TTerminalQueue * __fastcall TTerminalManager::FindQueueForTerminal(TTerminal * Terminal)
{
  int Index = IndexOf(Terminal);
  return reinterpret_cast<TTerminalQueue *>(FQueues->Items[Index]);
}
