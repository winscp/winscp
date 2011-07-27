//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Common.h"
#include "Terminal.h"
#include "Queue.h"
#include "Exceptions.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
class TBackgroundTerminal;
//---------------------------------------------------------------------------
class TUserAction
{
public:
  virtual __fastcall ~TUserAction() {}
  virtual void __fastcall Execute(TTerminalQueue * Queue, void * Arg) = 0;
};
//---------------------------------------------------------------------------
class TQueryUserAction : public TUserAction
{
public:
  virtual void __fastcall Execute(TTerminalQueue * Queue, void * Arg)
  {
    Queue->DoQueryUser(Sender, Query, MoreMessages, Answers, Params, Answer, Type, Arg);
  }

  TObject * Sender;
  AnsiString Query;
  TStrings * MoreMessages;
  int Answers;
  const TQueryParams * Params;
  int Answer;
  TQueryType Type;
};
//---------------------------------------------------------------------------
class TPromptUserAction : public TUserAction
{
public:
  __fastcall TPromptUserAction() :
    Results(new TStringList())
  {
  }

  virtual __fastcall ~TPromptUserAction()
  {
    delete Results;
  }

  virtual void __fastcall Execute(TTerminalQueue * Queue, void * Arg)
  {
    Queue->DoPromptUser(Terminal, Kind, Name, Instructions, Prompts, Results, Result, Arg);
  }

  TTerminal * Terminal;
  TPromptKind Kind;
  AnsiString Name;
  AnsiString Instructions;
  TStrings * Prompts;
  TStrings * Results;
  bool Result;
};
//---------------------------------------------------------------------------
class TShowExtendedExceptionAction : public TUserAction
{
public:
  virtual void __fastcall Execute(TTerminalQueue * Queue, void * Arg)
  {
    Queue->DoShowExtendedException(Terminal, E, Arg);
  }

  TTerminal * Terminal;
  Exception * E;
};
//---------------------------------------------------------------------------
class TTerminalItem : public TSignalThread
{
friend class TQueueItem;
friend class TBackgroundTerminal;

public:
  __fastcall TTerminalItem(TTerminalQueue * Queue, int Index);
  __fastcall ~TTerminalItem();

  void __fastcall Process(TQueueItem * Item);
  bool __fastcall ProcessUserAction(void * Arg);
  void __fastcall Cancel();
  void __fastcall Idle();
  bool __fastcall Pause();
  bool __fastcall Resume();

protected:
  TTerminalQueue * FQueue;
  TBackgroundTerminal * FTerminal;
  TQueueItem * FItem;
  TCriticalSection * FCriticalSection;
  TUserAction * FUserAction;
  bool FCancel;
  bool FPause;

  virtual void __fastcall ProcessEvent();
  virtual void __fastcall Finished();
  bool __fastcall WaitForUserAction(TQueueItem::TStatus ItemStatus, TUserAction * UserAction);
  bool __fastcall OverrideItemStatus(TQueueItem::TStatus & ItemStatus);

  void __fastcall TerminalQueryUser(TObject * Sender,
    const AnsiString Query, TStrings * MoreMessages, int Answers,
    const TQueryParams * Params, int & Answer, TQueryType Type, void * Arg);
  void __fastcall TerminalPromptUser(TTerminal * Terminal, TPromptKind Kind,
    AnsiString Name, AnsiString Instructions,
    TStrings * Prompts, TStrings * Results, bool & Result, void * Arg);
  void __fastcall TerminalShowExtendedException(TTerminal * Terminal,
    Exception * E, void * Arg);
  void __fastcall OperationFinished(TFileOperation Operation, TOperationSide Side,
    bool Temp, const AnsiString & FileName, bool Success,
    TOnceDoneOperation & OnceDoneOperation);
  void __fastcall OperationProgress(TFileOperationProgressType & ProgressData,
    TCancelStatus & Cancel);
};
//---------------------------------------------------------------------------
// TSignalThread
//---------------------------------------------------------------------------
int __fastcall TSimpleThread::ThreadProc(void * Thread)
{
  TSimpleThread * SimpleThread = reinterpret_cast<TSimpleThread*>(Thread);
  assert(SimpleThread != NULL);
  try
  {
    SimpleThread->Execute();
  }
  catch(...)
  {
    // we do not expect thread to be terminated with exception
    assert(false);
  }
  SimpleThread->FFinished = true;
  SimpleThread->Finished();
  return 0;
}
//---------------------------------------------------------------------------
__fastcall TSimpleThread::TSimpleThread() :
  FThread(NULL), FFinished(true)
{
  unsigned ThreadID;
  FThread = reinterpret_cast<HANDLE>(
    StartThread(NULL, 0, ThreadProc, this, CREATE_SUSPENDED, ThreadID));
}
//---------------------------------------------------------------------------
__fastcall TSimpleThread::~TSimpleThread()
{
  Close();

  if (FThread != NULL)
  {
    CloseHandle(FThread);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TSimpleThread::IsFinished()
{
  return FFinished;
}
//---------------------------------------------------------------------------
void __fastcall TSimpleThread::Start()
{
  if (ResumeThread(FThread) == 1)
  {
    FFinished = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSimpleThread::Finished()
{
}
//---------------------------------------------------------------------------
void __fastcall TSimpleThread::Close()
{
  if (!FFinished)
  {
    Terminate();
    WaitFor();
  }
}
//---------------------------------------------------------------------------
void __fastcall TSimpleThread::WaitFor(unsigned int Milliseconds)
{
  WaitForSingleObject(FThread, Milliseconds);
}
//---------------------------------------------------------------------------
// TSignalThread
//---------------------------------------------------------------------------
__fastcall TSignalThread::TSignalThread() :
  TSimpleThread(),
  FTerminated(true), FEvent(NULL)
{
  FEvent = CreateEvent(NULL, false, false, NULL);
  assert(FEvent != NULL);

  ::SetThreadPriority(FThread, THREAD_PRIORITY_BELOW_NORMAL);
}
//---------------------------------------------------------------------------
__fastcall TSignalThread::~TSignalThread()
{
  // cannot leave closing to TSimpleThread as we need to close it before
  // destroying the event
  Close();

  if (FEvent)
  {
    CloseHandle(FEvent);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSignalThread::Start()
{
  FTerminated = false;
  TSimpleThread::Start();
}
//---------------------------------------------------------------------------
void __fastcall TSignalThread::TriggerEvent()
{
  SetEvent(FEvent);
}
//---------------------------------------------------------------------------
bool __fastcall TSignalThread::WaitForEvent()
{
  return (WaitForSingleObject(FEvent, INFINITE) == WAIT_OBJECT_0) &&
    !FTerminated;
}
//---------------------------------------------------------------------------
void __fastcall TSignalThread::Execute()
{
  while (!FTerminated)
  {
    if (WaitForEvent())
    {
      ProcessEvent();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSignalThread::Terminate()
{
  FTerminated = true;
  TriggerEvent();
}
//---------------------------------------------------------------------------
// TTerminalQueue
//---------------------------------------------------------------------------
__fastcall TTerminalQueue::TTerminalQueue(TTerminal * Terminal,
  TConfiguration * Configuration) :
  FTerminal(Terminal), FTransfersLimit(2),
  FConfiguration(Configuration), FSessionData(NULL), FItems(NULL),
  FTerminals(NULL), FItemsSection(NULL), FFreeTerminals(0),
  FItemsInProcess(0), FTemporaryTerminals(0), FOverallTerminals(0)
{
  FOnQueryUser = NULL;
  FOnPromptUser = NULL;
  FOnShowExtendedException = NULL;
  FOnQueueItemUpdate = NULL;
  FOnListUpdate = NULL;
  FOnEvent = NULL;
  FLastIdle = Now();
  FIdleInterval = EncodeTimeVerbose(0, 0, 2, 0);

  assert(Terminal != NULL);
  FSessionData = new TSessionData("");
  FSessionData->Assign(Terminal->SessionData);

  FItems = new TList();
  FTerminals = new TList();

  FItemsSection = new TCriticalSection();

  Start();
}
//---------------------------------------------------------------------------
__fastcall TTerminalQueue::~TTerminalQueue()
{
  Close();

  {
    TGuard Guard(FItemsSection);

    TTerminalItem * TerminalItem;
    while (FTerminals->Count > 0)
    {
      TerminalItem = reinterpret_cast<TTerminalItem*>(FTerminals->Items[0]);
      FTerminals->Delete(0);
      TerminalItem->Terminate();
      TerminalItem->WaitFor();
      delete TerminalItem;
    }
    delete FTerminals;

    for (int Index = 0; Index < FItems->Count; Index++)
    {
      delete GetItem(Index);
    }
    delete FItems;
  }

  delete FItemsSection;
  delete FSessionData;
}
//---------------------------------------------------------------------------
void __fastcall TTerminalQueue::TerminalFinished(TTerminalItem * TerminalItem)
{
  if (!FTerminated)
  {
    {
      TGuard Guard(FItemsSection);

      int Index = FTerminals->IndexOf(TerminalItem);
      assert(Index >= 0);

      if (Index < FFreeTerminals)
      {
        FFreeTerminals--;
      }

      // Index may be >= FTransfersLimit also when the transfer limit was
      // recently decresed, then
      // FTemporaryTerminals < FTerminals->Count - FTransfersLimit
      if ((FTransfersLimit > 0) && (Index >= FTransfersLimit) && (FTemporaryTerminals > 0))
      {
        FTemporaryTerminals--;
      }

      FTerminals->Extract(TerminalItem);

      delete TerminalItem;
    }

    TriggerEvent();
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminalQueue::TerminalFree(TTerminalItem * TerminalItem)
{
  bool Result = true;

  if (!FTerminated)
  {
    {
      TGuard Guard(FItemsSection);

      int Index = FTerminals->IndexOf(TerminalItem);
      assert(Index >= 0);
      assert(Index >= FFreeTerminals);

      Result = (FTransfersLimit <= 0) || (Index < FTransfersLimit);
      if (Result)
      {
        FTerminals->Move(Index, 0);
        FFreeTerminals++;
      }
    }

    TriggerEvent();
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminalQueue::AddItem(TQueueItem * Item)
{
  assert(!FTerminated);

  Item->SetStatus(TQueueItem::qsPending);

  {
    TGuard Guard(FItemsSection);

    FItems->Add(Item);
    Item->FQueue = this;
  }

  DoListUpdate();

  TriggerEvent();
}
//---------------------------------------------------------------------------
void __fastcall TTerminalQueue::RetryItem(TQueueItem * Item)
{
  if (!FTerminated)
  {
    {
      TGuard Guard(FItemsSection);

      int Index = FItems->Remove(Item);
      assert(Index < FItemsInProcess);
      USEDPARAM(Index);
      FItemsInProcess--;
      FItems->Add(Item);
    }

    DoListUpdate();

    TriggerEvent();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalQueue::DeleteItem(TQueueItem * Item)
{
  if (!FTerminated)
  {
    bool Empty;
    bool Monitored;
    {
      TGuard Guard(FItemsSection);

      // does this need to be within guard?
      Monitored = (Item->CompleteEvent != INVALID_HANDLE_VALUE);
      int Index = FItems->Remove(Item);
      assert(Index < FItemsInProcess);
      USEDPARAM(Index);
      FItemsInProcess--;
      delete Item;

      Empty = true;
      Index = 0;
      while (Empty && (Index < FItems->Count))
      {
        Empty = (GetItem(Index)->CompleteEvent != INVALID_HANDLE_VALUE);
        Index++;
      }
    }

    DoListUpdate();
    // report empty, if queue is empty or only monitored items are pending.
    // do not report if current item was the last, but was monitored.
    if (!Monitored && Empty)
    {
      DoEvent(qeEmpty);
    }
  }
}
//---------------------------------------------------------------------------
TQueueItem * __fastcall TTerminalQueue::GetItem(int Index)
{
  return reinterpret_cast<TQueueItem*>(FItems->Items[Index]);
}
//---------------------------------------------------------------------------
TTerminalQueueStatus * __fastcall TTerminalQueue::CreateStatus(TTerminalQueueStatus * Current)
{
  TTerminalQueueStatus * Status = new TTerminalQueueStatus();
  try
  {
    try
    {
      TGuard Guard(FItemsSection);

      TQueueItem * Item;
      TQueueItemProxy * ItemProxy;
      for (int Index = 0; Index < FItems->Count; Index++)
      {
        Item = GetItem(Index);
        if (Current != NULL)
        {
          ItemProxy = Current->FindByQueueItem(Item);
        }
        else
        {
          ItemProxy = NULL;
        }

        if (ItemProxy != NULL)
        {
          Current->Delete(ItemProxy);
          Status->Add(ItemProxy);
          ItemProxy->Update();
        }
        else
        {
          Status->Add(new TQueueItemProxy(this, Item));
        }
      }
    }
    __finally
    {
      if (Current != NULL)
      {
        delete Current;
      }
    }
  }
  catch(...)
  {
    delete Status;
    throw;
  }

  return Status;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminalQueue::ItemGetData(TQueueItem * Item,
  TQueueItemProxy * Proxy)
{
  // to prevent deadlocks when closing queue from other thread
  bool Result = !FFinished;
  if (Result)
  {
    TGuard Guard(FItemsSection);

    Result = (FItems->IndexOf(Item) >= 0);
    if (Result)
    {
      Item->GetData(Proxy);
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminalQueue::ItemProcessUserAction(TQueueItem * Item, void * Arg)
{
  // to prevent deadlocks when closing queue from other thread
  bool Result = !FFinished;
  if (Result)
  {
    TTerminalItem * TerminalItem;

    {
      TGuard Guard(FItemsSection);

      Result = (FItems->IndexOf(Item) >= 0) &&
        TQueueItem::IsUserActionStatus(Item->Status);
      if (Result)
      {
        TerminalItem = Item->FTerminalItem;
      }
    }

    if (Result)
    {
      Result = TerminalItem->ProcessUserAction(Arg);
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminalQueue::ItemMove(TQueueItem * Item, TQueueItem * BeforeItem)
{
  // to prevent deadlocks when closing queue from other thread
  bool Result = !FFinished;
  if (Result)
  {
    {
      TGuard Guard(FItemsSection);

      int Index = FItems->IndexOf(Item);
      int IndexDest = FItems->IndexOf(BeforeItem);
      Result = (Index >= 0) && (IndexDest >= 0) &&
        (Item->GetStatus() == TQueueItem::qsPending) &&
        (BeforeItem->GetStatus() == TQueueItem::qsPending);
      if (Result)
      {
        FItems->Move(Index, IndexDest);
      }
    }

    if (Result)
    {
      DoListUpdate();
      TriggerEvent();
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminalQueue::ItemExecuteNow(TQueueItem * Item)
{
  // to prevent deadlocks when closing queue from other thread
  bool Result = !FFinished;
  if (Result)
  {
    {
      TGuard Guard(FItemsSection);

      int Index = FItems->IndexOf(Item);
      Result = (Index >= 0) && (Item->GetStatus() == TQueueItem::qsPending) &&
        // prevent double-initiation when "execute" is clicked twice too fast
        (Index >= FItemsInProcess);
      if (Result)
      {
        if (Index > FItemsInProcess)
        {
          FItems->Move(Index, FItemsInProcess);
        }

        if ((FTransfersLimit > 0) && (FTerminals->Count >= FTransfersLimit))
        {
          FTemporaryTerminals++;
        }
      }
    }

    if (Result)
    {
      DoListUpdate();
      TriggerEvent();
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminalQueue::ItemDelete(TQueueItem * Item)
{
  // to prevent deadlocks when closing queue from other thread
  bool Result = !FFinished;
  if (Result)
  {
    bool UpdateList = false;

    {
      TGuard Guard(FItemsSection);

      int Index = FItems->IndexOf(Item);
      Result = (Index >= 0);
      if (Result)
      {
        if (Item->Status == TQueueItem::qsPending)
        {
          FItems->Delete(Index);
          UpdateList = true;
        }
        else
        {
          Item->FTerminalItem->Cancel();
        }
      }
    }

    if (UpdateList)
    {
      DoListUpdate();
      TriggerEvent();
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminalQueue::ItemPause(TQueueItem * Item, bool Pause)
{
  // to prevent deadlocks when closing queue from other thread
  bool Result = !FFinished;
  if (Result)
  {
    TTerminalItem * TerminalItem;

    {
      TGuard Guard(FItemsSection);

      Result = (FItems->IndexOf(Item) >= 0) &&
        ((Pause && (Item->Status == TQueueItem::qsProcessing)) ||
         (!Pause && (Item->Status == TQueueItem::qsPaused)));
      if (Result)
      {
        TerminalItem = Item->FTerminalItem;
      }
    }

    if (Result)
    {
      if (Pause)
      {
        Result = TerminalItem->Pause();
      }
      else
      {
        Result = TerminalItem->Resume();
      }
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminalQueue::ItemSetCPSLimit(TQueueItem * Item, unsigned long CPSLimit)
{
  // to prevent deadlocks when closing queue from other thread
  bool Result = !FFinished;
  if (Result)
  {
    TGuard Guard(FItemsSection);

    Result = (FItems->IndexOf(Item) >= 0);
    if (Result)
    {
      Item->SetCPSLimit(CPSLimit);
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminalQueue::Idle()
{
  if (Now() - FLastIdle > FIdleInterval)
  {
    FLastIdle = FIdleInterval;
    TTerminalItem * TerminalItem = NULL;

    if (FFreeTerminals > 0)
    {
      TGuard Guard(FItemsSection);

      if (FFreeTerminals > 0)
      {
        // take the last free terminal, because TerminalFree() puts it to the
        // front, this ensures we cycle thru all free terminals
        TerminalItem = reinterpret_cast<TTerminalItem*>(FTerminals->Items[FFreeTerminals - 1]);
        FTerminals->Move(FFreeTerminals - 1, FTerminals->Count - 1);
        FFreeTerminals--;
      }
    }

    if (TerminalItem != NULL)
    {
      TerminalItem->Idle();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalQueue::ProcessEvent()
{
  TTerminalItem * TerminalItem;
  TQueueItem * Item;

  do
  {
    TerminalItem = NULL;
    Item = NULL;

    if (FItems->Count > FItemsInProcess)
    {
      TGuard Guard(FItemsSection);

      if ((FFreeTerminals == 0) &&
          ((FTransfersLimit <= 0) ||
           (FTerminals->Count < FTransfersLimit + FTemporaryTerminals)))
      {
        FOverallTerminals++;
        TerminalItem = new TTerminalItem(this, FOverallTerminals);
        FTerminals->Add(TerminalItem);
      }
      else if (FFreeTerminals > 0)
      {
        TerminalItem = reinterpret_cast<TTerminalItem*>(FTerminals->Items[0]);
        FTerminals->Move(0, FTerminals->Count - 1);
        FFreeTerminals--;
      }

      if (TerminalItem != NULL)
      {
        Item = GetItem(FItemsInProcess);
        FItemsInProcess++;
      }
    }

    if (TerminalItem != NULL)
    {
      TerminalItem->Process(Item);
    }
  }
  while (!FTerminated && (TerminalItem != NULL));
}
//---------------------------------------------------------------------------
void __fastcall TTerminalQueue::DoQueueItemUpdate(TQueueItem * Item)
{
  if (OnQueueItemUpdate != NULL)
  {
    OnQueueItemUpdate(this, Item);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalQueue::DoListUpdate()
{
  if (OnListUpdate != NULL)
  {
    OnListUpdate(this);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalQueue::DoEvent(TQueueEvent Event)
{
  if (OnEvent != NULL)
  {
    OnEvent(this, Event);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalQueue::DoQueryUser(TObject * Sender,
  const AnsiString Query, TStrings * MoreMessages, int Answers,
  const TQueryParams * Params, int & Answer, TQueryType Type, void * Arg)
{
  if (OnQueryUser != NULL)
  {
    OnQueryUser(Sender, Query, MoreMessages, Answers, Params, Answer, Type, Arg);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalQueue::DoPromptUser(TTerminal * Terminal,
  TPromptKind Kind, AnsiString Name, AnsiString Instructions,
  TStrings * Prompts, TStrings * Results, bool & Result, void * Arg)
{
  if (OnPromptUser != NULL)
  {
    OnPromptUser(Terminal, Kind, Name, Instructions, Prompts, Results, Result, Arg);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalQueue::DoShowExtendedException(
  TTerminal * Terminal, Exception * E, void * Arg)
{
  if (OnShowExtendedException != NULL)
  {
    OnShowExtendedException(Terminal, E, Arg);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalQueue::SetTransfersLimit(int value)
{
  if (FTransfersLimit != value)
  {
    {
      TGuard Guard(FItemsSection);

      if ((value > 0) && (value < FItemsInProcess))
      {
        FTemporaryTerminals = (FItemsInProcess - value);
      }
      else
      {
        FTemporaryTerminals = 0;
      }
      FTransfersLimit = value;
    }

    TriggerEvent();
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminalQueue::GetIsEmpty()
{
  TGuard Guard(FItemsSection);
  return (FItems->Count == 0);
}
//---------------------------------------------------------------------------
// TBackgroundItem
//---------------------------------------------------------------------------
class TBackgroundTerminal : public TSecondaryTerminal
{
friend class TTerminalItem;
public:
  __fastcall TBackgroundTerminal(TTerminal * MainTerminal,
    TSessionData * SessionData, TConfiguration * Configuration,
    TTerminalItem * Item, const AnsiString & Name);

protected:
  virtual bool __fastcall DoQueryReopen(Exception * E);

private:
  TTerminalItem * FItem;
};
//---------------------------------------------------------------------------
__fastcall TBackgroundTerminal::TBackgroundTerminal(TTerminal * MainTerminal,
    TSessionData * SessionData, TConfiguration * Configuration, TTerminalItem * Item,
    const AnsiString & Name) :
  TSecondaryTerminal(MainTerminal, SessionData, Configuration, Name), FItem(Item)
{
}
//---------------------------------------------------------------------------
bool __fastcall TBackgroundTerminal::DoQueryReopen(Exception * /*E*/)
{
  bool Result;
  if (FItem->FTerminated || FItem->FCancel)
  {
    // avoid reconnection if we are closing
    Result = false;
  }
  else
  {
    Sleep(Configuration->SessionReopenBackground);
    Result = true;
  }
  return Result;
}
//---------------------------------------------------------------------------
// TTerminalItem
//---------------------------------------------------------------------------
__fastcall TTerminalItem::TTerminalItem(TTerminalQueue * Queue, int Index) :
  TSignalThread(), FQueue(Queue), FTerminal(NULL), FItem(NULL),
  FCriticalSection(NULL), FUserAction(NULL)
{
  FCriticalSection = new TCriticalSection();

  FTerminal = new TBackgroundTerminal(FQueue->FTerminal, Queue->FSessionData,
    FQueue->FConfiguration, this, FORMAT("Background %d", (Index)));
  try
  {
    FTerminal->UseBusyCursor = false;

    FTerminal->OnQueryUser = TerminalQueryUser;
    FTerminal->OnPromptUser = TerminalPromptUser;
    FTerminal->OnShowExtendedException = TerminalShowExtendedException;
    FTerminal->OnProgress = OperationProgress;
    FTerminal->OnFinished = OperationFinished;
  }
  catch(...)
  {
    delete FTerminal;
    throw;
  }

  Start();
}
//---------------------------------------------------------------------------
__fastcall TTerminalItem::~TTerminalItem()
{
  Close();

  assert(FItem == NULL);
  delete FTerminal;
  delete FCriticalSection;
}
//---------------------------------------------------------------------------
void __fastcall TTerminalItem::Process(TQueueItem * Item)
{
  {
    TGuard Guard(FCriticalSection);

    assert(FItem == NULL);
    FItem = Item;
  }

  TriggerEvent();
}
//---------------------------------------------------------------------------
void __fastcall TTerminalItem::ProcessEvent()
{
  TGuard Guard(FCriticalSection);

  bool Retry = true;

  FCancel = false;
  FPause = false;
  FItem->FTerminalItem = this;

  try
  {
    assert(FItem != NULL);

    if (!FTerminal->Active)
    {
      FItem->SetStatus(TQueueItem::qsConnecting);

      FTerminal->SessionData->RemoteDirectory = FItem->StartupDirectory();
      FTerminal->Open();
    }

    Retry = false;

    if (!FCancel)
    {
      FItem->SetStatus(TQueueItem::qsProcessing);

      FItem->Execute(this);
    }
  }
  catch(Exception & E)
  {
    // do not show error messages, if task was canceled anyway
    // (for example if transfer is cancelled during reconnection attempts)
    if (!FCancel &&
        (FTerminal->QueryUserException("", &E, qaOK | qaCancel, NULL, qtError) == qaCancel))
    {
      FCancel = true;
    }
  }

  FItem->SetStatus(TQueueItem::qsDone);

  FItem->FTerminalItem = NULL;

  TQueueItem * Item = FItem;
  FItem = NULL;

  if (Retry && !FCancel)
  {
    FQueue->RetryItem(Item);
  }
  else
  {
    FQueue->DeleteItem(Item);
  }

  if (!FTerminal->Active ||
      !FQueue->TerminalFree(this))
  {
    Terminate();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalItem::Idle()
{
  TGuard Guard(FCriticalSection);

  assert(FTerminal->Active);

  try
  {
    FTerminal->Idle();
  }
  catch(...)
  {
  }

  if (!FTerminal->Active ||
      !FQueue->TerminalFree(this))
  {
    Terminate();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalItem::Cancel()
{
  FCancel = true;
  if ((FItem->GetStatus() == TQueueItem::qsPaused) ||
      TQueueItem::IsUserActionStatus(FItem->GetStatus()))
  {
    TriggerEvent();
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminalItem::Pause()
{
  assert(FItem != NULL);
  bool Result = (FItem->GetStatus() == TQueueItem::qsProcessing) && !FPause;
  if (Result)
  {
    FPause = true;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminalItem::Resume()
{
  assert(FItem != NULL);
  bool Result = (FItem->GetStatus() == TQueueItem::qsPaused);
  if (Result)
  {
    TriggerEvent();
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminalItem::ProcessUserAction(void * Arg)
{
  // When status is changed twice quickly, the controller when responding
  // to the first change (non-user-action) can be so slow to check only after
  // the second (user-action) change occurs. Thus it responds it.
  // Then as reaction to the second (user-action) change there will not be
  // any outstanding user-action.
  bool Result = (FUserAction != NULL);
  if (Result)
  {
    assert(FItem != NULL);

    FUserAction->Execute(FQueue, Arg);
    FUserAction = NULL;

    TriggerEvent();
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminalItem::WaitForUserAction(
  TQueueItem::TStatus ItemStatus, TUserAction * UserAction)
{
  assert(FItem != NULL);
  assert((FItem->GetStatus() == TQueueItem::qsProcessing) ||
    (FItem->GetStatus() == TQueueItem::qsConnecting));

  bool Result;

  TQueueItem::TStatus PrevStatus = FItem->GetStatus();

  try
  {
    FUserAction = UserAction;

    FItem->SetStatus(ItemStatus);
    FQueue->DoEvent(qePendingUserAction);

    Result = !FTerminated && WaitForEvent() && !FCancel;
  }
  __finally
  {
    FUserAction = NULL;
    FItem->SetStatus(PrevStatus);
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminalItem::Finished()
{
  TSignalThread::Finished();

  FQueue->TerminalFinished(this);
}
//---------------------------------------------------------------------------
void __fastcall TTerminalItem::TerminalQueryUser(TObject * Sender,
  const AnsiString Query, TStrings * MoreMessages, int Answers,
  const TQueryParams * Params, int & Answer, TQueryType Type, void * Arg)
{
  // so far query without queue item can occur only for key cofirmation
  // on re-key with non-cached host key. make it fail.
  if (FItem != NULL)
  {
    USEDPARAM(Arg);
    assert(Arg == NULL);

    TQueryUserAction Action;
    Action.Sender = Sender;
    Action.Query = Query;
    Action.MoreMessages = MoreMessages;
    Action.Answers = Answers;
    Action.Params = Params;
    Action.Answer = Answer;
    Action.Type = Type;

    // if the query is "error", present it as an "error" state in UI,
    // however it is still handled as query by the action.

    TQueueItem::TStatus ItemStatus =
      (Action.Type == qtError ? TQueueItem::qsError : TQueueItem::qsQuery);

    if (WaitForUserAction(ItemStatus, &Action))
    {
      Answer = Action.Answer;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalItem::TerminalPromptUser(TTerminal * Terminal,
  TPromptKind Kind, AnsiString Name, AnsiString Instructions, TStrings * Prompts,
  TStrings * Results, bool & Result, void * Arg)
{
  if (FItem == NULL)
  {
    // sanity, should not occur
    assert(false);
    Result = false;
  }
  else
  {
    USEDPARAM(Arg);
    assert(Arg == NULL);

    TPromptUserAction Action;
    Action.Terminal = Terminal;
    Action.Kind = Kind;
    Action.Name = Name;
    Action.Instructions = Instructions;
    Action.Prompts = Prompts;
    Action.Results->AddStrings(Results);

    if (WaitForUserAction(TQueueItem::qsPrompt, &Action))
    {
      Results->Clear();
      Results->AddStrings(Action.Results);
      Result = Action.Result;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalItem::TerminalShowExtendedException(
  TTerminal * Terminal, Exception * E, void * Arg)
{
  USEDPARAM(Arg);
  assert(Arg == NULL);

  AnsiString Message; // not used
  if ((FItem != NULL) &&
      ExceptionMessage(E, Message))
  {
    TShowExtendedExceptionAction Action;
    Action.Terminal = Terminal;
    Action.E = E;

    WaitForUserAction(TQueueItem::qsError, &Action);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalItem::OperationFinished(TFileOperation /*Operation*/,
  TOperationSide /*Side*/, bool /*Temp*/, const AnsiString & /*FileName*/,
  bool /*Success*/, TOnceDoneOperation & /*OnceDoneOperation*/)
{
  // nothing
}
//---------------------------------------------------------------------------
void __fastcall TTerminalItem::OperationProgress(
  TFileOperationProgressType & ProgressData, TCancelStatus & Cancel)
{
  if (FPause && !FTerminated && !FCancel)
  {
    TQueueItem::TStatus PrevStatus = FItem->GetStatus();
    assert(PrevStatus == TQueueItem::qsProcessing);
    // must be set before TFileOperationProgressType::Suspend(), because
    // it invokes this method back
    FPause = false;
    ProgressData.Suspend();

    try
    {
      FItem->SetStatus(TQueueItem::qsPaused);

      WaitForEvent();
    }
    __finally
    {
      FItem->SetStatus(PrevStatus);
      ProgressData.Resume();
    }
  }

  if (FTerminated || FCancel)
  {
    if (ProgressData.TransferingFile)
    {
      Cancel = csCancelTransfer;
    }
    else
    {
      Cancel = csCancel;
    }
  }

  assert(FItem != NULL);
  FItem->SetProgress(ProgressData);
}
//---------------------------------------------------------------------------
bool __fastcall TTerminalItem::OverrideItemStatus(TQueueItem::TStatus & ItemStatus)
{
  assert(FTerminal != NULL);
  bool Result = (FTerminal->Status < ssOpened) && (ItemStatus == TQueueItem::qsProcessing);
  if (Result)
  {
    ItemStatus = TQueueItem::qsConnecting;
  }
  return Result;
}
//---------------------------------------------------------------------------
// TQueueItem
//---------------------------------------------------------------------------
__fastcall TQueueItem::TQueueItem() :
  FStatus(qsPending), FTerminalItem(NULL), FSection(NULL), FProgressData(NULL),
  FQueue(NULL), FInfo(NULL), FCompleteEvent(INVALID_HANDLE_VALUE),
  FCPSLimit(-1)
{
  FSection = new TCriticalSection();
  FInfo = new TInfo();
}
//---------------------------------------------------------------------------
__fastcall TQueueItem::~TQueueItem()
{
  if (FCompleteEvent != INVALID_HANDLE_VALUE)
  {
    SetEvent(FCompleteEvent);
  }

  delete FSection;
  delete FInfo;
}
//---------------------------------------------------------------------------
bool __fastcall TQueueItem::IsUserActionStatus(TStatus Status)
{
  return (Status == qsQuery) || (Status == qsError) || (Status == qsPrompt);
}
//---------------------------------------------------------------------------
TQueueItem::TStatus __fastcall TQueueItem::GetStatus()
{
  TGuard Guard(FSection);

  return FStatus;
}
//---------------------------------------------------------------------------
void __fastcall TQueueItem::SetStatus(TStatus Status)
{
  {
    TGuard Guard(FSection);

    FStatus = Status;
  }

  assert((FQueue != NULL) || (Status == qsPending));
  if (FQueue != NULL)
  {
    FQueue->DoQueueItemUpdate(this);
  }
}
//---------------------------------------------------------------------------
void __fastcall TQueueItem::SetProgress(
  TFileOperationProgressType & ProgressData)
{
  {
    TGuard Guard(FSection);

    assert(FProgressData != NULL);
    *FProgressData = ProgressData;
    FProgressData->Reset();

    if (FCPSLimit >= 0)
    {
      ProgressData.CPSLimit = static_cast<unsigned long>(FCPSLimit);
      FCPSLimit = -1;
    }
  }
  FQueue->DoQueueItemUpdate(this);
}
//---------------------------------------------------------------------------
void __fastcall TQueueItem::GetData(TQueueItemProxy * Proxy)
{
  TGuard Guard(FSection);

  assert(Proxy->FProgressData != NULL);
  if (FProgressData != NULL)
  {
    *Proxy->FProgressData = *FProgressData;
  }
  else
  {
    Proxy->FProgressData->Clear();
  }
  *Proxy->FInfo = *FInfo;
  Proxy->FStatus = FStatus;
  if (FTerminalItem != NULL)
  {
    FTerminalItem->OverrideItemStatus(Proxy->FStatus);
  }
}
//---------------------------------------------------------------------------
void __fastcall TQueueItem::Execute(TTerminalItem * TerminalItem)
{
  try
  {
    {
      assert(FProgressData == NULL);
      TGuard Guard(FSection);
      FProgressData = new TFileOperationProgressType();
    }
    DoExecute(TerminalItem->FTerminal);
  }
  __finally
  {
    {
      TGuard Guard(FSection);
      delete FProgressData;
      FProgressData = NULL;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TQueueItem::SetCPSLimit(unsigned long CPSLimit)
{
  FCPSLimit = static_cast<long>(CPSLimit);
}
//---------------------------------------------------------------------------
// TQueueItemProxy
//---------------------------------------------------------------------------
__fastcall TQueueItemProxy::TQueueItemProxy(TTerminalQueue * Queue,
  TQueueItem * QueueItem) :
  FQueue(Queue), FQueueItem(QueueItem), FProgressData(NULL),
  FQueueStatus(NULL), FInfo(NULL),
  FProcessingUserAction(false), FUserData(NULL)
{
  FProgressData = new TFileOperationProgressType();
  FInfo = new TQueueItem::TInfo();

  Update();
}
//---------------------------------------------------------------------------
__fastcall TQueueItemProxy::~TQueueItemProxy()
{
  delete FProgressData;
  delete FInfo;
}
//---------------------------------------------------------------------------
TFileOperationProgressType * __fastcall TQueueItemProxy::GetProgressData()
{
  return (FProgressData->Operation == foNone) ? NULL : FProgressData;
}
//---------------------------------------------------------------------------
bool __fastcall TQueueItemProxy::Update()
{
  assert(FQueueItem != NULL);

  TQueueItem::TStatus PrevStatus = Status;

  bool Result = FQueue->ItemGetData(FQueueItem, this);

  if ((FQueueStatus != NULL) && (PrevStatus != Status))
  {
    FQueueStatus->ResetStats();
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TQueueItemProxy::ExecuteNow()
{
  return FQueue->ItemExecuteNow(FQueueItem);
}
//---------------------------------------------------------------------------
bool __fastcall TQueueItemProxy::Move(bool Sooner)
{
  bool Result = false;
  int I = Index;
  if (Sooner)
  {
    if (I > 0)
    {
      Result = Move(FQueueStatus->Items[I - 1]);
    }
  }
  else
  {
    if (I < FQueueStatus->Count - 1)
    {
      Result = FQueueStatus->Items[I + 1]->Move(this);
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TQueueItemProxy::Move(TQueueItemProxy * BeforeItem)
{
  return FQueue->ItemMove(FQueueItem, BeforeItem->FQueueItem);
}
//---------------------------------------------------------------------------
bool __fastcall TQueueItemProxy::Delete()
{
  return FQueue->ItemDelete(FQueueItem);
}
//---------------------------------------------------------------------------
bool __fastcall TQueueItemProxy::Pause()
{
  return FQueue->ItemPause(FQueueItem, true);
}
//---------------------------------------------------------------------------
bool __fastcall TQueueItemProxy::Resume()
{
  return FQueue->ItemPause(FQueueItem, false);
}
//---------------------------------------------------------------------------
bool __fastcall TQueueItemProxy::ProcessUserAction(void * Arg)
{
  assert(FQueueItem != NULL);

  bool Result;
  FProcessingUserAction = true;
  try
  {
    Result = FQueue->ItemProcessUserAction(FQueueItem, Arg);
  }
  __finally
  {
    FProcessingUserAction = false;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TQueueItemProxy::SetCPSLimit(unsigned long CPSLimit)
{
  return FQueue->ItemSetCPSLimit(FQueueItem, CPSLimit);
}
//---------------------------------------------------------------------------
int __fastcall TQueueItemProxy::GetIndex()
{
  assert(FQueueStatus != NULL);
  int Index = FQueueStatus->FList->IndexOf(this);
  assert(Index >= 0);
  return Index;
}
//---------------------------------------------------------------------------
// TTerminalQueueStatus
//---------------------------------------------------------------------------
__fastcall TTerminalQueueStatus::TTerminalQueueStatus() :
  FList(NULL)
{
  FList = new TList();
  ResetStats();
}
//---------------------------------------------------------------------------
__fastcall TTerminalQueueStatus::~TTerminalQueueStatus()
{
  for (int Index = 0; Index < FList->Count; Index++)
  {
    delete GetItem(Index);
  }
  delete FList;
  FList = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TTerminalQueueStatus::ResetStats()
{
  FActiveCount = -1;
}
//---------------------------------------------------------------------------
int __fastcall TTerminalQueueStatus::GetActiveCount()
{
  if (FActiveCount < 0)
  {
    FActiveCount = 0;

    while ((FActiveCount < FList->Count) &&
      (GetItem(FActiveCount)->Status != TQueueItem::qsPending))
    {
      FActiveCount++;
    }
  }

  return FActiveCount;
}
//---------------------------------------------------------------------------
void __fastcall TTerminalQueueStatus::Add(TQueueItemProxy * ItemProxy)
{
  ItemProxy->FQueueStatus = this;
  FList->Add(ItemProxy);
  ResetStats();
}
//---------------------------------------------------------------------------
void __fastcall TTerminalQueueStatus::Delete(TQueueItemProxy * ItemProxy)
{
  FList->Extract(ItemProxy);
  ItemProxy->FQueueStatus = NULL;
  ResetStats();
}
//---------------------------------------------------------------------------
int __fastcall TTerminalQueueStatus::GetCount()
{
  return FList->Count;
}
//---------------------------------------------------------------------------
TQueueItemProxy * __fastcall TTerminalQueueStatus::GetItem(int Index)
{
  return reinterpret_cast<TQueueItemProxy *>(FList->Items[Index]);
}
//---------------------------------------------------------------------------
TQueueItemProxy * __fastcall TTerminalQueueStatus::FindByQueueItem(
  TQueueItem * QueueItem)
{
  TQueueItemProxy * Item;
  for (int Index = 0; Index < FList->Count; Index++)
  {
    Item = GetItem(Index);
    if (Item->FQueueItem == QueueItem)
    {
      return Item;
    }
  }
  return NULL;
}
//---------------------------------------------------------------------------
// TLocatedQueueItem
//---------------------------------------------------------------------------
__fastcall TLocatedQueueItem::TLocatedQueueItem(TTerminal * Terminal) :
  TQueueItem()
{
  assert(Terminal != NULL);
  FCurrentDir = Terminal->CurrentDirectory;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TLocatedQueueItem::StartupDirectory()
{
  return FCurrentDir;
}
//---------------------------------------------------------------------------
void __fastcall TLocatedQueueItem::DoExecute(TTerminal * Terminal)
{
  assert(Terminal != NULL);
  Terminal->CurrentDirectory = FCurrentDir;
}
//---------------------------------------------------------------------------
// TTransferQueueItem
//---------------------------------------------------------------------------
__fastcall TTransferQueueItem::TTransferQueueItem(TTerminal * Terminal,
  TStrings * FilesToCopy, const AnsiString & TargetDir,
  const TCopyParamType * CopyParam, int Params, TOperationSide Side) :
  TLocatedQueueItem(Terminal), FFilesToCopy(NULL), FCopyParam(NULL)
{
  FInfo->Operation = (Params & cpDelete ? foMove : foCopy);
  FInfo->Side = Side;

  assert(FilesToCopy != NULL);
  FFilesToCopy = new TStringList();
  for (int Index = 0; Index < FilesToCopy->Count; Index++)
  {
    FFilesToCopy->AddObject(FilesToCopy->Strings[Index],
      ((FilesToCopy->Objects[Index] == NULL) || (Side == osLocal)) ? NULL :
        dynamic_cast<TRemoteFile*>(FilesToCopy->Objects[Index])->Duplicate());
  }

  FTargetDir = TargetDir;

  assert(CopyParam != NULL);
  FCopyParam = new TCopyParamType(*CopyParam);

  FParams = Params;
}
//---------------------------------------------------------------------------
__fastcall TTransferQueueItem::~TTransferQueueItem()
{
  for (int Index = 0; Index < FFilesToCopy->Count; Index++)
  {
    delete FFilesToCopy->Objects[Index];
  }
  delete FFilesToCopy;
  delete FCopyParam;
}
//---------------------------------------------------------------------------
// TUploadQueueItem
//---------------------------------------------------------------------------
__fastcall TUploadQueueItem::TUploadQueueItem(TTerminal * Terminal,
  TStrings * FilesToCopy, const AnsiString & TargetDir,
  const TCopyParamType * CopyParam, int Params) :
  TTransferQueueItem(Terminal, FilesToCopy, TargetDir, CopyParam, Params, osLocal)
{
  if (FilesToCopy->Count > 1)
  {
    if (FLAGSET(Params, cpTemporary))
    {
      FInfo->Source = "";
      FInfo->ModifiedLocal = "";
    }
    else
    {
      ExtractCommonPath(FilesToCopy, FInfo->Source);
      // this way the trailing backslash is preserved for root directories like D:\\
      FInfo->Source = ExtractFileDir(IncludeTrailingBackslash(FInfo->Source));
      FInfo->ModifiedLocal = FLAGCLEAR(Params, cpDelete) ? AnsiString() :
        IncludeTrailingBackslash(FInfo->Source);
    }
  }
  else
  {
    if (FLAGSET(Params, cpTemporary))
    {
      FInfo->Source = ExtractFileName(FilesToCopy->Strings[0]);
      FInfo->ModifiedLocal = "";
    }
    else
    {
      assert(FilesToCopy->Count > 0);
      FInfo->Source = FilesToCopy->Strings[0];
      FInfo->ModifiedLocal = FLAGCLEAR(Params, cpDelete) ? AnsiString() :
        IncludeTrailingBackslash(ExtractFilePath(FInfo->Source));
    }
  }

  FInfo->Destination =
    UnixIncludeTrailingBackslash(TargetDir) + CopyParam->FileMask;
  FInfo->ModifiedRemote = UnixIncludeTrailingBackslash(TargetDir);
}
//---------------------------------------------------------------------------
void __fastcall TUploadQueueItem::DoExecute(TTerminal * Terminal)
{
  TTransferQueueItem::DoExecute(Terminal);

  assert(Terminal != NULL);
  Terminal->CopyToRemote(FFilesToCopy, FTargetDir, FCopyParam, FParams);
}
//---------------------------------------------------------------------------
// TDownloadQueueItem
//---------------------------------------------------------------------------
__fastcall TDownloadQueueItem::TDownloadQueueItem(TTerminal * Terminal,
  TStrings * FilesToCopy, const AnsiString & TargetDir,
  const TCopyParamType * CopyParam, int Params) :
  TTransferQueueItem(Terminal, FilesToCopy, TargetDir, CopyParam, Params, osRemote)
{
  if (FilesToCopy->Count > 1)
  {
    if (!UnixExtractCommonPath(FilesToCopy, FInfo->Source))
    {
      FInfo->Source = Terminal->CurrentDirectory;
    }
    FInfo->Source = UnixExcludeTrailingBackslash(FInfo->Source);
    FInfo->ModifiedRemote = FLAGCLEAR(Params, cpDelete) ? AnsiString() :
      UnixIncludeTrailingBackslash(FInfo->Source);
  }
  else
  {
    assert(FilesToCopy->Count > 0);
    FInfo->Source = FilesToCopy->Strings[0];
    if (UnixExtractFilePath(FInfo->Source).IsEmpty())
    {
      FInfo->Source = UnixIncludeTrailingBackslash(Terminal->CurrentDirectory) +
        FInfo->Source;
      FInfo->ModifiedRemote = FLAGCLEAR(Params, cpDelete) ? AnsiString() :
        UnixIncludeTrailingBackslash(Terminal->CurrentDirectory);
    }
    else
    {
      FInfo->ModifiedRemote = FLAGCLEAR(Params, cpDelete) ? AnsiString() :
        UnixExtractFilePath(FInfo->Source);
    }
  }

  if (FLAGSET(Params, cpTemporary))
  {
    FInfo->Destination = "";
  }
  else
  {
    FInfo->Destination =
      IncludeTrailingBackslash(TargetDir) + CopyParam->FileMask;
  }
  FInfo->ModifiedLocal = IncludeTrailingBackslash(TargetDir);
}
//---------------------------------------------------------------------------
void __fastcall TDownloadQueueItem::DoExecute(TTerminal * Terminal)
{
  TTransferQueueItem::DoExecute(Terminal);

  assert(Terminal != NULL);
  Terminal->CopyToLocal(FFilesToCopy, FTargetDir, FCopyParam, FParams);
}
