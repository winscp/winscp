//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Common.h"
#include "Terminal.h"
#include "Queue.h"
#include "Exceptions.h"
#include <System.DateUtils.hpp>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
class TBackgroundTerminal;
//---------------------------------------------------------------------------
class TParallelTransferQueueItem : public TLocatedQueueItem
{
public:
  __fastcall TParallelTransferQueueItem(const TLocatedQueueItem * ParentItem, TParallelOperation * ParallelOperation);

protected:
  virtual void __fastcall DoExecute(TTerminal * Terminal);

private:
  TParallelOperation * FParallelOperation;
};
//---------------------------------------------------------------------------
class TUserAction
{
public:
  virtual __fastcall ~TUserAction() {}
  virtual void __fastcall Execute(void * Arg) = 0;
  virtual bool __fastcall Force() { return false; }
};
//---------------------------------------------------------------------------
class TNotifyAction : public TUserAction
{
public:
  TNotifyAction(TNotifyEvent AOnNotify) :
    OnNotify(AOnNotify)
  {
  }

  virtual void __fastcall Execute(void *)
  {
    if (OnNotify != NULL)
    {
      OnNotify(Sender);
    }
  }

  TNotifyEvent OnNotify;
  TObject * Sender;
};
//---------------------------------------------------------------------------
class TInformationUserAction : public TUserAction
{
public:
  TInformationUserAction(TInformationEvent AOnInformation) :
    OnInformation(AOnInformation)
  {
  }

  virtual void __fastcall Execute(void *)
  {
    if (OnInformation != NULL)
    {
      OnInformation(Terminal, Str, Phase, Additional);
    }
  }

  virtual bool __fastcall Force()
  {
    // we need to propagate mainly the end-phase event even, when user cancels
    // the connection, so that authentication window is closed
    return TUserAction::Force() || (Phase >= 0);
  }

  TInformationEvent OnInformation;
  TTerminal * Terminal;
  UnicodeString Str;
  int Phase;
  UnicodeString Additional;
};
//---------------------------------------------------------------------------
class TQueryUserAction : public TUserAction
{
public:
  TQueryUserAction(TQueryUserEvent AOnQueryUser) :
    OnQueryUser(AOnQueryUser)
  {
  }

  virtual void __fastcall Execute(void * Arg)
  {
    if (OnQueryUser != NULL)
    {
      OnQueryUser(Sender, Query, MoreMessages, Answers, Params, Answer, Type, Arg);
    }
  }

  TQueryUserEvent OnQueryUser;
  TObject * Sender;
  UnicodeString Query;
  TStrings * MoreMessages;
  unsigned int Answers;
  const TQueryParams * Params;
  unsigned int Answer;
  TQueryType Type;
};
//---------------------------------------------------------------------------
class TPromptUserAction : public TUserAction
{
public:
  __fastcall TPromptUserAction(TPromptUserEvent AOnPromptUser) :
    OnPromptUser(AOnPromptUser), Results(new TStringList())
  {
  }

  virtual __fastcall ~TPromptUserAction()
  {
    delete Results;
  }

  virtual void __fastcall Execute(void * Arg)
  {
    if (OnPromptUser != NULL)
    {
      OnPromptUser(Terminal, Kind, Name, Instructions, Prompts, Results, Result, Arg);
    }
  }

  TPromptUserEvent OnPromptUser;
  TTerminal * Terminal;
  TPromptKind Kind;
  UnicodeString Name;
  UnicodeString Instructions;
  TStrings * Prompts;
  TStrings * Results;
  bool Result;
};
//---------------------------------------------------------------------------
class TShowExtendedExceptionAction : public TUserAction
{
public:
  __fastcall TShowExtendedExceptionAction(TExtendedExceptionEvent AOnShowExtendedException) :
    OnShowExtendedException(AOnShowExtendedException)
  {
  }

  virtual void __fastcall Execute(void * Arg)
  {
    if (OnShowExtendedException != NULL)
    {
      OnShowExtendedException(Terminal, E, Arg);
    }
  }

  TExtendedExceptionEvent OnShowExtendedException;
  TTerminal * Terminal;
  Exception * E;
};
//---------------------------------------------------------------------------
class TDisplayBannerAction : public TUserAction
{
public:
  __fastcall TDisplayBannerAction (TDisplayBannerEvent AOnDisplayBanner) :
    OnDisplayBanner(AOnDisplayBanner)
  {
  }

  virtual void __fastcall Execute(void *)
  {
    if (OnDisplayBanner != NULL)
    {
      OnDisplayBanner(Terminal, SessionName, Banner, NeverShowAgain, Options, Params);
    }
  }

  TDisplayBannerEvent OnDisplayBanner;
  TTerminal * Terminal;
  UnicodeString SessionName;
  UnicodeString Banner;
  bool NeverShowAgain;
  int Options;
  unsigned int Params;
};
//---------------------------------------------------------------------------
class TReadDirectoryAction : public TUserAction
{
public:
  TReadDirectoryAction(TReadDirectoryEvent AOnReadDirectory) :
    OnReadDirectory(AOnReadDirectory)
  {
  }

  virtual void __fastcall Execute(void *)
  {
    if (OnReadDirectory != NULL)
    {
      OnReadDirectory(Sender, ReloadOnly);
    }
  }

  TReadDirectoryEvent OnReadDirectory;
  TObject * Sender;
  bool ReloadOnly;
};
//---------------------------------------------------------------------------
class TReadDirectoryProgressAction : public TUserAction
{
public:
  TReadDirectoryProgressAction(TReadDirectoryProgressEvent AOnReadDirectoryProgress) :
    OnReadDirectoryProgress(AOnReadDirectoryProgress)
  {
  }

  virtual void __fastcall Execute(void *)
  {
    if (OnReadDirectoryProgress != NULL)
    {
      OnReadDirectoryProgress(Sender, Progress, ResolvedLinks, Cancel);
    }
  }

  TReadDirectoryProgressEvent OnReadDirectoryProgress;
  TObject * Sender;
  int Progress;
  int ResolvedLinks;
  bool Cancel;
};
//---------------------------------------------------------------------------
class TTerminalItem : public TSignalThread
{
friend class TQueueItem;
friend class TBackgroundTerminal;

public:
  __fastcall TTerminalItem(TTerminalQueue * Queue, int Index);
  virtual __fastcall ~TTerminalItem();

  void __fastcall Process(TQueueItem * Item);
  bool __fastcall ProcessUserAction(void * Arg);
  void __fastcall Cancel();
  void __fastcall Idle();
  bool __fastcall Pause();
  bool __fastcall Resume();
  bool IsCancelled();

protected:
  TTerminalQueue * FQueue;
  TBackgroundTerminal * FTerminal;
  TQueueItem * FItem;
  TCriticalSection * FCriticalSection;
  TUserAction * FUserAction;
  bool FCancel;
  bool FPause;

  virtual void __fastcall ProcessEvent();
  virtual bool __fastcall Finished();
  bool __fastcall WaitForUserAction(TQueueItem::TStatus ItemStatus, TUserAction * UserAction);
  bool __fastcall OverrideItemStatus(TQueueItem::TStatus & ItemStatus);

  void __fastcall TerminalQueryUser(TObject * Sender,
    const UnicodeString Query, TStrings * MoreMessages, unsigned int Answers,
    const TQueryParams * Params, unsigned int & Answer, TQueryType Type, void * Arg);
  void __fastcall TerminalPromptUser(TTerminal * Terminal, TPromptKind Kind,
    UnicodeString Name, UnicodeString Instructions,
    TStrings * Prompts, TStrings * Results, bool & Result, void * Arg);
  void __fastcall TerminalShowExtendedException(TTerminal * Terminal,
    Exception * E, void * Arg);
  void __fastcall OperationFinished(TFileOperation Operation, TOperationSide Side,
    bool Temp, const UnicodeString & FileName, bool Success, bool NotCancelled,
    TOnceDoneOperation & OnceDoneOperation);
  void __fastcall OperationProgress(TFileOperationProgressType & ProgressData);
};
//---------------------------------------------------------------------------
// TSignalThread
//---------------------------------------------------------------------------
int __fastcall TSimpleThread::ThreadProc(void * Thread)
{
  TSimpleThread * SimpleThread = reinterpret_cast<TSimpleThread*>(Thread);
  DebugAssert(SimpleThread != NULL);
  try
  {
    SimpleThread->Execute();
  }
  catch(...)
  {
    // we do not expect thread to be terminated with exception
    DebugFail();
  }
  SimpleThread->FFinished = true;
  if (SimpleThread->Finished())
  {
    delete SimpleThread;
  }
  return 0;
}
//---------------------------------------------------------------------------
__fastcall TSimpleThread::TSimpleThread() :
  FThread(NULL), FFinished(true)
{
  FThread = reinterpret_cast<HANDLE>(
    StartThread(NULL, 0, ThreadProc, this, CREATE_SUSPENDED, FThreadId));
}
//---------------------------------------------------------------------------
__fastcall TSimpleThread::~TSimpleThread()
{
  // This is turn calls pure virtual Terminate, what does not work as intended, do not rely on it and remove the call eventually
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
bool __fastcall TSimpleThread::Finished()
{
  return false;
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
__fastcall TSignalThread::TSignalThread(bool LowPriority, HANDLE Event) :
  TSimpleThread(),
  FEvent(NULL), FTerminated(true)
{
  if (Event == NULL)
  {
    FEvent = CreateEvent(NULL, false, false, NULL);
  }
  else
  {
    FEvent = Event;
  }
  DebugAssert(FEvent != NULL);

  if (LowPriority)
  {
    ::SetThreadPriority(FThread, THREAD_PRIORITY_BELOW_NORMAL);
  }
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
  // should never return -1, so it is only about 0 or 1
  return (WaitForEvent(INFINITE) > 0);
}
//---------------------------------------------------------------------------
int __fastcall TSignalThread::WaitForEvent(unsigned int Timeout)
{
  unsigned int Result = WaitForSingleObject(FEvent, Timeout);
  int Return;
  if ((Result == WAIT_TIMEOUT) && !FTerminated)
  {
    Return = -1;
  }
  else
  {
    Return = ((Result == WAIT_OBJECT_0) && !FTerminated) ? 1 : 0;
  }
  return Return;
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
  TSignalThread(true), FTerminal(Terminal), FConfiguration(Configuration), FSessionData(NULL), FItems(NULL), FDoneItems(NULL),
  FItemsInProcess(0), FItemsSection(NULL), FFreeTerminals(0), FTerminals(NULL), FTemporaryTerminals(0),
  FOverallTerminals(0), FTransfersLimit(2), FKeepDoneItemsFor(0), FEnabled(true)
{
  FOnQueryUser = NULL;
  FOnPromptUser = NULL;
  FOnShowExtendedException = NULL;
  FOnQueueItemUpdate = NULL;
  FOnListUpdate = NULL;
  FOnEvent = NULL;
  FLastIdle = Now();
  FIdleInterval = EncodeTimeVerbose(0, 0, 2, 0);

  DebugAssert(Terminal != NULL);
  FSessionData = new TSessionData(L"");
  FSessionData->Assign(Terminal->SessionData);

  FItems = new TList();
  FDoneItems = new TList();
  FTerminals = new TList();
  FForcedItems = new TList();

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
    delete FForcedItems;

    FreeItemsList(FItems);
    FreeItemsList(FDoneItems);
  }

  delete FItemsSection;
  delete FSessionData;
}
//---------------------------------------------------------------------------
void __fastcall TTerminalQueue::FreeItemsList(TList * List)
{
  for (int Index = 0; Index < List->Count; Index++)
  {
    delete GetItem(List, Index);
  }
  delete List;
}
//---------------------------------------------------------------------------
void __fastcall TTerminalQueue::TerminalFinished(TTerminalItem * TerminalItem)
{
  if (!FTerminated)
  {
    {
      TGuard Guard(FItemsSection);

      int Index = FTerminals->IndexOf(TerminalItem);
      DebugAssert(Index >= 0);

      if (Index < FFreeTerminals)
      {
        FFreeTerminals--;
      }

      // Index may be >= FTransfersLimit also when the transfer limit was
      // recently decreased, then
      // FTemporaryTerminals < FTerminals->Count - FTransfersLimit
      if ((FTransfersLimit >= 0) && (Index >= FTransfersLimit) && (FTemporaryTerminals > 0))
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
      DebugAssert(Index >= 0);
      DebugAssert(Index >= FFreeTerminals);

      Result = (FTransfersLimit < 0) || (Index < FTransfersLimit);
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
int __fastcall TTerminalQueue::GetParallelDurationThreshold()
{
  return FConfiguration->ParallelDurationThreshold;
}
//---------------------------------------------------------------------------
void __fastcall TTerminalQueue::AddItem(TQueueItem * Item)
{
  DebugAssert(!FTerminated);

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
      DebugAssert(Index < FItemsInProcess);
      DebugUsedParam(Index);
      FItemsInProcess--;
      FItems->Add(Item);
    }

    DoListUpdate();

    TriggerEvent();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalQueue::DeleteItem(TQueueItem * Item, bool CanKeep)
{
  if (!FTerminated)
  {
    bool Empty;
    bool EmptyButMonitored;
    bool Monitored;
    {
      TGuard Guard(FItemsSection);

      // does this need to be within guard?
      Monitored = (Item->CompleteEvent != INVALID_HANDLE_VALUE);
      int Index = FItems->Remove(Item);
      DebugAssert(Index < FItemsInProcess);
      DebugUsedParam(Index);
      FItemsInProcess--;
      FForcedItems->Remove(Item);
      // =0  do not keep
      // <0  infinity
      if ((FKeepDoneItemsFor != 0) && CanKeep && Item->Complete())
      {
        DebugAssert(Item->Status == TQueueItem::qsDone);
        FDoneItems->Add(Item);
      }
      else
      {
        delete Item;
      }

      EmptyButMonitored = true;
      Index = 0;
      while (EmptyButMonitored && (Index < FItems->Count))
      {
        EmptyButMonitored = (GetItem(FItems, Index)->CompleteEvent != INVALID_HANDLE_VALUE);
        Index++;
      }
      Empty = (FItems->Count == 0);
    }

    DoListUpdate();
    // report empty but/except for monitored, if queue is empty or only monitored items are pending.
    // do not report if current item was the last, but was monitored.
    if (!Monitored && EmptyButMonitored)
    {
      DoEvent(qeEmptyButMonitored);
    }
    if (Empty)
    {
      DoEvent(qeEmpty);
    }
  }
}
//---------------------------------------------------------------------------
TQueueItem * __fastcall TTerminalQueue::GetItem(TList * List, int Index)
{
  return reinterpret_cast<TQueueItem*>(List->Items[Index]);
}
//---------------------------------------------------------------------------
TQueueItem * __fastcall TTerminalQueue::GetItem(int Index)
{
  return GetItem(FItems, Index);
}
//---------------------------------------------------------------------------
void __fastcall TTerminalQueue::UpdateStatusForList(
  TTerminalQueueStatus * Status, TList * List, TTerminalQueueStatus * Current)
{
  TQueueItem * Item;
  TQueueItemProxy * ItemProxy;
  for (int Index = 0; Index < List->Count; Index++)
  {
    Item = GetItem(List, Index);
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
//---------------------------------------------------------------------------
TTerminalQueueStatus * __fastcall TTerminalQueue::CreateStatus(TTerminalQueueStatus * Current)
{
  TTerminalQueueStatus * Status = new TTerminalQueueStatus();
  try
  {
    try
    {
      TGuard Guard(FItemsSection);

      UpdateStatusForList(Status, FDoneItems, Current);
      Status->SetDoneCount(Status->Count);
      UpdateStatusForList(Status, FItems, Current);
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
bool __fastcall TTerminalQueue::ItemGetData(TQueueItem * Item, TQueueItemProxy * Proxy, TQueueFileList * FileList)
{
  // to prevent deadlocks when closing queue from other thread
  bool Result = !FFinished;
  if (Result)
  {
    TGuard Guard(FItemsSection);

    Result = (FDoneItems->IndexOf(Item) >= 0) || (FItems->IndexOf(Item) >= 0);
    if (Result)
    {
      if (FileList != NULL)
      {
        Result = Item->UpdateFileList(FileList);
      }
      else
      {
        Item->GetData(Proxy);
      }
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
    TTerminalItem * TerminalItem = NULL; // shut up

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

        if ((FTransfersLimit >= 0) && (FTerminals->Count >= FTransfersLimit) &&
            // when queue is disabled, we may have idle terminals,
            // even when there are pending queue items
            (FFreeTerminals == 0))
        {
          FTemporaryTerminals++;
        }

        FForcedItems->Add(Item);
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
          FForcedItems->Remove(Item);
          delete Item;
          UpdateList = true;
        }
        else
        {
          Item->FTerminalItem->Cancel();
        }
      }
      else
      {
        Index = FDoneItems->IndexOf(Item);
        Result = (Index >= 0);
        if (Result)
        {
          FDoneItems->Delete(Index);
          delete Item;
          UpdateList = true;
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
    TTerminalItem * TerminalItem = NULL; // shut up

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
bool __fastcall TTerminalQueue::ItemGetCPSLimit(TQueueItem * Item, unsigned long & CPSLimit)
{
  CPSLimit = 0;
  // to prevent deadlocks when closing queue from other thread
  bool Result = !FFinished;
  if (Result)
  {
    TGuard Guard(FItemsSection);

    Result = (FItems->IndexOf(Item) >= 0);
    if (Result)
    {
      CPSLimit = Item->GetCPSLimit();
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminalQueue::Idle()
{
  TDateTime N = Now();
  if (N - FLastIdle > FIdleInterval)
  {
    FLastIdle = N;
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
bool __fastcall TTerminalQueue::WaitForEvent()
{
  // terminate loop regularly, so that we can check for expired done items
  bool Result = (TSignalThread::WaitForEvent(1000) != 0);
  return Result;
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

    {
      TGuard Guard(FItemsSection);

      // =0  do not keep
      // <0  infinity
      if (FKeepDoneItemsFor >= 0)
      {
        TDateTime RemoveDoneItemsBefore = Now();
        if (FKeepDoneItemsFor > 0)
        {
          RemoveDoneItemsBefore = IncSecond(RemoveDoneItemsBefore, -FKeepDoneItemsFor);
        }
        for (int Index = 0; Index < FDoneItems->Count; Index++)
        {
          TQueueItem * Item = GetItem(FDoneItems, Index);
          if (Item->FDoneAt <= RemoveDoneItemsBefore)
          {
            FDoneItems->Delete(Index);
            delete Item;
            Index--;
            DoListUpdate();
          }
        }
      }

      if (FItems->Count > FItemsInProcess)
      {
        Item = GetItem(FItemsInProcess);
        int ForcedIndex = FForcedItems->IndexOf(Item);

        if (FEnabled || (ForcedIndex >= 0))
        {
          if ((FFreeTerminals == 0) &&
              ((FTransfersLimit < 0) ||
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
            if (ForcedIndex >= 0)
            {
              FForcedItems->Delete(ForcedIndex);
            }
            FItemsInProcess++;
          }
        }
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
void __fastcall TTerminalQueue::SetTransfersLimit(int value)
{
  if (FTransfersLimit != value)
  {
    {
      TGuard Guard(FItemsSection);

      if ((value >= 0) && (value < FItemsInProcess))
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
void __fastcall TTerminalQueue::SetKeepDoneItemsFor(int value)
{
  if (FKeepDoneItemsFor != value)
  {
    {
      TGuard Guard(FItemsSection);

      FKeepDoneItemsFor = value;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalQueue::SetEnabled(bool value)
{
  if (FEnabled != value)
  {
    {
      TGuard Guard(FItemsSection);

      FEnabled = value;
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
bool __fastcall TTerminalQueue::TryAddParallelOperation(TQueueItem * Item, bool Force)
{
  TGuard Guard(FItemsSection);

  bool Result =
    (FFreeTerminals > 0) ||
    (Force && (FItemsInProcess < FTransfersLimit));

  if (Result)
  {
    TQueueItem * ParallelItem = DebugNotNull(Item->CreateParallelOperation());
    if (!FEnabled)
    {
      FForcedItems->Add(ParallelItem);
    }
    AddItem(ParallelItem);
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminalQueue::ContinueParallelOperation()
{
  TGuard Guard(FItemsSection);

  return
    (FItems->Count <= FItemsInProcess) ||
    // When queue auto processing is not enabled, keep using all connections for the transfers that were manually started
    !FEnabled;
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
    TTerminalItem * Item, const UnicodeString & Name);

protected:
  virtual bool __fastcall DoQueryReopen(Exception * E);

private:
  TTerminalItem * FItem;
};
//---------------------------------------------------------------------------
__fastcall TBackgroundTerminal::TBackgroundTerminal(TTerminal * MainTerminal,
    TSessionData * SessionData, TConfiguration * Configuration, TTerminalItem * Item,
    const UnicodeString & Name) :
  TSecondaryTerminal(MainTerminal, SessionData, Configuration, Name, NULL), FItem(Item)
{
  ActionLog->Enabled = false;
}
//---------------------------------------------------------------------------
bool __fastcall TBackgroundTerminal::DoQueryReopen(Exception * /*E*/)
{
  bool Result;
  if (FItem->IsCancelled())
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
  TSignalThread(true), FQueue(Queue), FTerminal(NULL), FItem(NULL),
  FCriticalSection(NULL), FUserAction(NULL)
{
  FCriticalSection = new TCriticalSection();

  FTerminal = new TBackgroundTerminal(FQueue->FTerminal, Queue->FSessionData,
    FQueue->FConfiguration, this, FORMAT(L"Background %d", (Index)));
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

  DebugAssert(FItem == NULL);
  delete FTerminal;
  delete FCriticalSection;
}
//---------------------------------------------------------------------------
void __fastcall TTerminalItem::Process(TQueueItem * Item)
{
  {
    TGuard Guard(FCriticalSection);

    DebugAssert(FItem == NULL);
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
    DebugAssert(FItem != NULL);

    if (!FTerminal->Active)
    {
      FItem->SetStatus(TQueueItem::qsConnecting);

      FTerminal->SessionData->RemoteDirectory = FItem->StartupDirectory();
      FTerminal->Open();
    }

    Retry = false;

    if (!FCancel)
    {
      FTerminal->UpdateFromMain();

      FItem->SetStatus(TQueueItem::qsProcessing);

      FItem->Execute();
    }
  }
  catch(Exception & E)
  {
    UnicodeString Message;
    if (ExceptionMessageFormatted(&E, Message))
    {
      // do not show error messages, if task was canceled anyway
      // (for example if transfer is canceled during reconnection attempts)
      if (!FCancel &&
          (FTerminal->QueryUserException(L"", &E, qaOK | qaCancel, NULL, qtError) == qaCancel))
      {
        FCancel = true;
      }
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
    FQueue->DeleteItem(Item, !FCancel);
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

  DebugAssert(FTerminal->Active);

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
  DebugAssert(FItem != NULL);
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
  DebugAssert(FItem != NULL);
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
    DebugAssert(FItem != NULL);

    FUserAction->Execute(Arg);
    FUserAction = NULL;

    TriggerEvent();
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminalItem::WaitForUserAction(
  TQueueItem::TStatus ItemStatus, TUserAction * UserAction)
{
  DebugAssert(FItem != NULL);
  DebugAssert((FItem->GetStatus() == TQueueItem::qsProcessing) ||
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
bool __fastcall TTerminalItem::Finished()
{
  bool Result = TSignalThread::Finished();

  FQueue->TerminalFinished(this);

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminalItem::TerminalQueryUser(TObject * Sender,
  const UnicodeString Query, TStrings * MoreMessages, unsigned int Answers,
  const TQueryParams * Params, unsigned int & Answer, TQueryType Type, void * Arg)
{
  // so far query without queue item can occur only for key confirmation
  // on re-key with non-cached host key. make it fail.
  if (FItem != NULL)
  {
    DebugUsedParam(Arg);
    DebugAssert(Arg == NULL);

    TQueryUserAction Action(FQueue->OnQueryUser);
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
  TPromptKind Kind, UnicodeString Name, UnicodeString Instructions, TStrings * Prompts,
  TStrings * Results, bool & Result, void * Arg)
{
  if (FItem == NULL)
  {
    // sanity, should not occur
    DebugFail();
    Result = false;
  }
  else
  {
    DebugUsedParam(Arg);
    DebugAssert(Arg == NULL);

    TPromptUserAction Action(FQueue->OnPromptUser);
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
  DebugUsedParam(Arg);
  DebugAssert(Arg == NULL);

  if ((FItem != NULL) &&
      ShouldDisplayException(E))
  {
    TShowExtendedExceptionAction Action(FQueue->OnShowExtendedException);
    Action.Terminal = Terminal;
    Action.E = E;

    WaitForUserAction(TQueueItem::qsError, &Action);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalItem::OperationFinished(TFileOperation /*Operation*/,
  TOperationSide /*Side*/, bool /*Temp*/, const UnicodeString & /*FileName*/,
  bool /*Success*/, bool /*NotCancelled*/, TOnceDoneOperation & /*OnceDoneOperation*/)
{
  // nothing
}
//---------------------------------------------------------------------------
void __fastcall TTerminalItem::OperationProgress(
  TFileOperationProgressType & ProgressData)
{
  if (FPause && !IsCancelled())
  {
    DebugAssert(FItem != NULL);
    TQueueItem::TStatus PrevStatus = FItem->GetStatus();
    DebugAssert(PrevStatus == TQueueItem::qsProcessing);
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

  if (IsCancelled())
  {
    if (ProgressData.TransferringFile)
    {
      ProgressData.SetCancel(csCancelTransfer);
    }
    else
    {
      ProgressData.SetCancel(csCancel);
    }
  }

  DebugAssert(FItem != NULL);
  FItem->SetProgress(ProgressData);
}
//---------------------------------------------------------------------------
bool __fastcall TTerminalItem::OverrideItemStatus(TQueueItem::TStatus & ItemStatus)
{
  DebugAssert(FTerminal != NULL);
  bool Result = (FTerminal->Status < ssOpened) && (ItemStatus == TQueueItem::qsProcessing);
  if (Result)
  {
    ItemStatus = TQueueItem::qsConnecting;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool TTerminalItem::IsCancelled()
{
  return FTerminated || FCancel;
}
//---------------------------------------------------------------------------
// TQueueItem
//---------------------------------------------------------------------------
__fastcall TQueueItem::TQueueItem() :
  FStatus(qsPending), FSection(NULL), FTerminalItem(NULL), FProgressData(NULL),
  FInfo(NULL), FQueue(NULL), FCompleteEvent(INVALID_HANDLE_VALUE),
  FCPSLimit(-1)
{
  FSection = new TCriticalSection();
  FInfo = new TInfo();
  FInfo->SingleFile = false;
  FInfo->Primary = true;
  FInfo->GroupToken = this;
}
//---------------------------------------------------------------------------
__fastcall TQueueItem::~TQueueItem()
{
  // we need to keep the total transfer size even after transfer completes
  delete FProgressData;

  Complete();

  delete FSection;
  delete FInfo;
}
//---------------------------------------------------------------------------
bool __fastcall TQueueItem::Complete()
{
  TGuard Guard(FSection);

  if (FCompleteEvent != INVALID_HANDLE_VALUE)
  {
    SetEvent(FCompleteEvent);
    FCompleteEvent = INVALID_HANDLE_VALUE;
  }

  return FInfo->Primary;
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
    if (FStatus == qsDone)
    {
      FDoneAt = Now();
    }
  }

  DebugAssert((FQueue != NULL) || (Status == qsPending));
  if (FQueue != NULL)
  {
    FQueue->DoQueueItemUpdate(this);
  }
}
//---------------------------------------------------------------------------
void __fastcall TQueueItem::ProgressUpdated()
{
  // noop
}
//---------------------------------------------------------------------------
void __fastcall TQueueItem::SetProgress(
  TFileOperationProgressType & ProgressData)
{
  {
    TGuard Guard(FSection);

    // do not lose CPS limit override on "calculate size" operation,
    // wait until the real transfer operation starts
    if ((FCPSLimit >= 0) && ProgressData.IsTransfer())
    {
      ProgressData.SetCPSLimit(static_cast<unsigned long>(FCPSLimit));
      FCPSLimit = -1;
    }

    DebugAssert(FProgressData != NULL);
    FProgressData->Assign(ProgressData);
    FProgressData->Reset();
  }
  ProgressUpdated();
  FQueue->DoQueueItemUpdate(this);
}
//---------------------------------------------------------------------------
void __fastcall TQueueItem::GetData(TQueueItemProxy * Proxy)
{
  TGuard Guard(FSection);

  DebugAssert(Proxy->FProgressData != NULL);
  if (FProgressData != NULL)
  {
    Proxy->FProgressData->Assign(*FProgressData);
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
bool __fastcall TQueueItem::UpdateFileList(TQueueFileList *)
{
  // noop - implemented in TTransferQueueItem
  return false;
}
//---------------------------------------------------------------------------
bool TQueueItem::IsExecutionCancelled()
{
  return DebugAlwaysTrue(FTerminalItem != NULL) ? FTerminalItem->IsCancelled() : true;
}
//---------------------------------------------------------------------------
void __fastcall TQueueItem::Execute()
{
  {
    DebugAssert(FProgressData == NULL);
    TGuard Guard(FSection);
    FProgressData = new TFileOperationProgressType();
  }
  DoExecute(FTerminalItem->FTerminal);
}
//---------------------------------------------------------------------------
void __fastcall TQueueItem::SetCPSLimit(unsigned long CPSLimit)
{
  FCPSLimit = static_cast<long>(CPSLimit);
}
//---------------------------------------------------------------------------
unsigned long __fastcall TQueueItem::DefaultCPSLimit()
{
  return 0;
}
//---------------------------------------------------------------------------
unsigned long __fastcall TQueueItem::GetCPSLimit()
{
  unsigned long Result;
  if (FCPSLimit >= 0)
  {
    Result = FCPSLimit;
  }
  else if (FProgressData != NULL)
  {
    Result = FProgressData->CPSLimit;
  }
  else
  {
    Result = DefaultCPSLimit();
  }
  return Result;
}
//---------------------------------------------------------------------------
TQueueItem * __fastcall TQueueItem::CreateParallelOperation()
{
  return NULL;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TQueueItem::StartupDirectory() const
{
  return EmptyStr;
}
//---------------------------------------------------------------------------
// TQueueItemProxy
//---------------------------------------------------------------------------
__fastcall TQueueItemProxy::TQueueItemProxy(TTerminalQueue * Queue,
  TQueueItem * QueueItem) :
  UserData(NULL), FProgressData(NULL), FQueue(Queue), FQueueItem(QueueItem),
  FQueueStatus(NULL), FInfo(NULL), FProcessingUserAction(false)
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
__int64 __fastcall TQueueItemProxy::GetTotalTransferred()
{
  // want to show total transferred also for "completed" items,
  // for which GetProgressData() is NULL
  return
    (FProgressData->Operation == Info->Operation) || (Status == TQueueItem::qsDone) ?
      FProgressData->TotalTransferred : -1;
}
//---------------------------------------------------------------------------
bool __fastcall TQueueItemProxy::Update()
{
  DebugAssert(FQueueItem != NULL);

  TQueueItem::TStatus PrevStatus = Status;

  bool Result = FQueue->ItemGetData(FQueueItem, this, NULL);

  if ((FQueueStatus != NULL) && (PrevStatus != Status))
  {
    FQueueStatus->ResetStats();
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TQueueItemProxy::UpdateFileList(TQueueFileList * FileList)
{
  return FQueue->ItemGetData(FQueueItem, NULL, FileList);
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
bool __fastcall TQueueItemProxy::ProcessUserAction()
{
  DebugAssert(FQueueItem != NULL);

  bool Result;
  FProcessingUserAction = true;
  try
  {
    Result = FQueue->ItemProcessUserAction(FQueueItem, NULL);
  }
  __finally
  {
    FProcessingUserAction = false;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TQueueItemProxy::GetCPSLimit(unsigned long & CPSLimit)
{
  return FQueue->ItemGetCPSLimit(FQueueItem, CPSLimit);
}
//---------------------------------------------------------------------------
bool __fastcall TQueueItemProxy::SetCPSLimit(unsigned long CPSLimit)
{
  return FQueue->ItemSetCPSLimit(FQueueItem, CPSLimit);
}
//---------------------------------------------------------------------------
int __fastcall TQueueItemProxy::GetIndex()
{
  DebugAssert(FQueueStatus != NULL);
  int Index = FQueueStatus->FList->IndexOf(this);
  DebugAssert(Index >= 0);
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
  FActivePrimaryCount = -1;
  FActiveAndPendingPrimaryCount = -1;
}
//---------------------------------------------------------------------------
void __fastcall TTerminalQueueStatus::SetDoneCount(int Value)
{
  FDoneCount = Value;
  ResetStats();
}
//---------------------------------------------------------------------------
void __fastcall TTerminalQueueStatus::NeedStats()
{
  if (FActiveCount < 0)
  {
    FActiveCount = 0;
    FActivePrimaryCount = 0;
    FActiveAndPendingPrimaryCount = 0;
    for (int Index = DoneCount; Index < Count; Index++)
    {
      bool Primary = GetItem(Index)->Info->Primary;

      if (GetItem(Index)->Status != TQueueItem::qsPending)
      {
        FActiveCount++;
        if (Primary)
        {
          FActivePrimaryCount++;
        }
      }
      if (Primary)
      {
        FActiveAndPendingPrimaryCount++;
      }
    }
  }
}
//---------------------------------------------------------------------------
int __fastcall TTerminalQueueStatus::GetActiveCount()
{
  NeedStats();
  return FActiveCount;
}
//---------------------------------------------------------------------------
int __fastcall TTerminalQueueStatus::GetDoneAndActiveCount()
{
  return DoneCount + ActiveCount;
}
//---------------------------------------------------------------------------
int __fastcall TTerminalQueueStatus::GetActivePrimaryCount()
{
  NeedStats();
  return FActivePrimaryCount;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminalQueueStatus::IsOnlyOneActiveAndNoPending()
{
  return (ActivePrimaryCount == 1) && (ActiveAndPendingPrimaryCount == 1);
}
//---------------------------------------------------------------------------
int __fastcall TTerminalQueueStatus::GetActiveAndPendingPrimaryCount()
{
  NeedStats();
  return FActiveAndPendingPrimaryCount;
}
//---------------------------------------------------------------------------
void __fastcall TTerminalQueueStatus::Add(TQueueItemProxy * ItemProxy)
{
  ItemProxy->FQueueStatus = this;

  int Index = FList->Count;
  if (!ItemProxy->Info->Primary)
  {
    for (int I = 0; I < FList->Count; I++)
    {
      if (Items[I]->Info->GroupToken == ItemProxy->Info->GroupToken)
      {
        Index = I + 1;
      }
    }

    DebugAssert(Index >= 0);
  }

  FList->Insert(Index, ItemProxy);
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
bool __fastcall TTerminalQueueStatus::UpdateFileList(TQueueItemProxy * ItemProxy, TQueueFileList * FileList)
{
  bool Result;
  if (ItemProxy != NULL)
  {
    Result = ItemProxy->UpdateFileList(FileList);
  }
  else
  {
    Result = (FileList->GetCount() > 0);
    if (Result)
    {
      FileList->Clear();
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
// TBootstrapQueueItem
//---------------------------------------------------------------------------
__fastcall TBootstrapQueueItem::TBootstrapQueueItem()
{
  FInfo->SingleFile = true;
}
//---------------------------------------------------------------------------
void __fastcall TBootstrapQueueItem::DoExecute(TTerminal * DebugUsedArg(Terminal))
{
  // noop
}
//---------------------------------------------------------------------------
bool __fastcall TBootstrapQueueItem::Complete()
{
  TQueueItem::Complete();
  // To hide the item, even if "keep done items" is on
  return false;
}
//---------------------------------------------------------------------------
// TLocatedQueueItem
//---------------------------------------------------------------------------
__fastcall TLocatedQueueItem::TLocatedQueueItem(TTerminal * Terminal) :
  TQueueItem()
{
  DebugAssert(Terminal != NULL);
  FCurrentDir = Terminal->CurrentDirectory;
}
//---------------------------------------------------------------------------
__fastcall TLocatedQueueItem::TLocatedQueueItem(const TLocatedQueueItem & Source) :
  TQueueItem()
{
  FCurrentDir = Source.FCurrentDir;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TLocatedQueueItem::StartupDirectory() const
{
  return FCurrentDir;
}
//---------------------------------------------------------------------------
void __fastcall TLocatedQueueItem::DoExecute(TTerminal * Terminal)
{
  DebugAssert(Terminal != NULL);
  Terminal->CurrentDirectory = FCurrentDir;
}
//---------------------------------------------------------------------------
// TTransferQueueItem
//---------------------------------------------------------------------------
__fastcall TTransferQueueItem::TTransferQueueItem(TTerminal * Terminal,
  TStrings * FilesToCopy, const UnicodeString & TargetDir,
  const TCopyParamType * CopyParam, int Params, TOperationSide Side,
  bool SingleFile, bool Parallel) :
  TLocatedQueueItem(Terminal), FFilesToCopy(NULL), FCopyParam(NULL)
{
  FInfo->Operation = (Params & cpDelete ? foMove : foCopy);
  FInfo->Side = Side;
  FInfo->SingleFile = SingleFile;

  if (FilesToCopy != NULL)
  {
    FFilesToCopy = new TStringList();
    for (int Index = 0; Index < FilesToCopy->Count; Index++)
    {
      UnicodeString FileName = FilesToCopy->Strings[Index];
      TRemoteFile * File = dynamic_cast<TRemoteFile *>(FilesToCopy->Objects[Index]);
      FFilesToCopy->AddObject(FileName, ((File == NULL) || (Side == osLocal)) ? NULL : File->Duplicate());
    }
  }

  FTargetDir = TargetDir;

  DebugAssert(CopyParam != NULL);
  FCopyParam = new TCopyParamType(*CopyParam);

  FParams = Params;

  FParallel = Parallel;
  FLastParallelOperationAdded = GetTickCount();

}
//---------------------------------------------------------------------------
__fastcall TTransferQueueItem::~TTransferQueueItem()
{
  if (FFilesToCopy != NULL)
  {
    for (int Index = 0; Index < FFilesToCopy->Count; Index++)
    {
      delete FFilesToCopy->Objects[Index];
    }
    delete FFilesToCopy;
  }
  delete FCopyParam;
}
//---------------------------------------------------------------------------
unsigned long __fastcall TTransferQueueItem::DefaultCPSLimit()
{
  return FCopyParam->CPSLimit;
}
//---------------------------------------------------------------------------
void __fastcall TTransferQueueItem::DoExecute(TTerminal * Terminal)
{
  TLocatedQueueItem::DoExecute(Terminal);

  DebugAssert(Terminal != NULL);
  FParallelOperation.reset(new TParallelOperation(FInfo->Side));
  try
  {
    DoTransferExecute(Terminal, FParallelOperation.get());
  }
  __finally
  {
    FParallelOperation->WaitFor();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTransferQueueItem::ProgressUpdated()
{
  TLocatedQueueItem::ProgressUpdated();

  if (FParallel)
  {
    bool Add = false;
    bool Force = false;
    DWORD LastParallelOperationAddedPrev = 0;

    {
      TGuard Guard(FSection);
      DebugAssert(FParallelOperation.get() != NULL);
      // Won't be initialized, if the operation is not eligible for parallel transfers (like cpDelete).
      // We can probably move the check outside of the guard.
      if (FParallelOperation->IsInitialized())
      {
        DebugAssert(FProgressData->IsTransfer());
        if (FProgressData->Operation == foCopy)
        {
          Add = FParallelOperation->ShouldAddClient();
          if (Add)
          {
            DWORD Now = GetTickCount();
            Force =
              (Now - FLastParallelOperationAdded >= 5*1000) &&
              (TimeToSeconds(FProgressData->TotalTimeLeft()) >= FQueue->ParallelDurationThreshold);
            LastParallelOperationAddedPrev = FLastParallelOperationAdded;
            // update now already to prevent race condition, but we will have to rollback it back,
            // if we actually do not add the parallel operation
            FLastParallelOperationAdded = Now;
          }
        }
      }
    }

    if (Add)
    {
      if (!FQueue->TryAddParallelOperation(this, Force))
      {
        TGuard Guard(FSection);
        FLastParallelOperationAdded = LastParallelOperationAddedPrev;
      }
    }
  }
}
//---------------------------------------------------------------------------
TQueueItem * __fastcall TTransferQueueItem::CreateParallelOperation()
{
  DebugAssert(FParallelOperation.get() != NULL);

  FParallelOperation->AddClient();
  DebugAssert(!FInfo->SingleFile || FParallelOperation->IsParallelFileTransfer);
  FInfo->SingleFile = false;
  return new TParallelTransferQueueItem(this, FParallelOperation.get());
}
//---------------------------------------------------------------------------
bool __fastcall TTransferQueueItem::UpdateFileList(TQueueFileList * FileList)
{
  TGuard Guard(FSection);
  bool Result;
  if ((FParallelOperation.get() != NULL) && FParallelOperation->IsInitialized())
  {
    Result = FParallelOperation->UpdateFileList(FileList);
  }
  else
  {
    Result = false;
  }
  return Result;
}
//---------------------------------------------------------------------------
// TUploadQueueItem
//---------------------------------------------------------------------------
static void ExtractLocalSourcePath(TStrings * Files, UnicodeString & Path)
{
  ExtractCommonPath(Files, Path);
  // this way the trailing backslash is preserved for root directories like "D:\"
  Path = ExtractFileDir(IncludeTrailingBackslash(Path));
}
//---------------------------------------------------------------------------
static bool IsSingleFileUpload(TStrings * FilesToCopy)
{
  return
    (FilesToCopy->Count == 1) &&
    FileExists(ApiPath(FilesToCopy->Strings[0]));
}
//---------------------------------------------------------------------------
__fastcall TUploadQueueItem::TUploadQueueItem(
    TTerminal * Terminal, TStrings * FilesToCopy, const UnicodeString & TargetDir,
    const TCopyParamType * CopyParam, int Params, bool Parallel) :
  TTransferQueueItem(
    Terminal, FilesToCopy, TargetDir, CopyParam, Params, osLocal, IsSingleFileUpload(FilesToCopy), Parallel)
{
  if (FilesToCopy->Count > 1)
  {
    if (FLAGSET(Params, cpTemporary))
    {
      FInfo->Source = L"";
      FInfo->ModifiedLocal = L"";
    }
    else
    {
      ExtractLocalSourcePath(FilesToCopy, FInfo->Source);
      FInfo->ModifiedLocal = FLAGCLEAR(Params, cpDelete) ? UnicodeString() :
        IncludeTrailingBackslash(FInfo->Source);
    }
  }
  else
  {
    if (FLAGSET(Params, cpTemporary))
    {
      FInfo->Source = ExtractFileName(FilesToCopy->Strings[0]);
      FInfo->ModifiedLocal = L"";
    }
    else
    {
      DebugAssert(FilesToCopy->Count > 0);
      FInfo->Source = FilesToCopy->Strings[0];
      FInfo->ModifiedLocal = FLAGCLEAR(Params, cpDelete) ? UnicodeString() :
        IncludeTrailingBackslash(ExtractFilePath(FInfo->Source));
    }
  }

  FInfo->Destination =
    UnixIncludeTrailingBackslash(TargetDir) + CopyParam->FileMask;
  FInfo->ModifiedRemote = UnixIncludeTrailingBackslash(TargetDir);
}
//---------------------------------------------------------------------------
void __fastcall TUploadQueueItem::DoTransferExecute(TTerminal * Terminal, TParallelOperation * ParallelOperation)
{
  Terminal->CopyToRemote(FFilesToCopy, FTargetDir, FCopyParam, FParams, ParallelOperation);
}
//---------------------------------------------------------------------------
// TParallelTransferQueueItem
//---------------------------------------------------------------------------
__fastcall TParallelTransferQueueItem::TParallelTransferQueueItem(
    const TLocatedQueueItem * ParentItem, TParallelOperation * ParallelOperation) :
  TLocatedQueueItem(*ParentItem),
  FParallelOperation(ParallelOperation)
{
  // deliberately not copying the ModifiedLocal and ModifiedRemote, not to trigger panel refresh, when sub-item completes
  FInfo->Operation = ParentItem->FInfo->Operation;
  FInfo->Side = ParentItem->FInfo->Side;
  FInfo->Source = ParentItem->FInfo->Source;
  FInfo->Destination = ParentItem->FInfo->Destination;
  FInfo->SingleFile = DebugAlwaysFalse(ParentItem->FInfo->SingleFile);
  FInfo->Primary = false;
  FInfo->GroupToken = ParentItem->FInfo->GroupToken;
}
//---------------------------------------------------------------------------
void __fastcall TParallelTransferQueueItem::DoExecute(TTerminal * Terminal)
{
  TLocatedQueueItem::DoExecute(Terminal);

  Terminal->LogParallelTransfer(FParallelOperation);
  TFileOperationProgressType OperationProgress(Terminal->OnProgress, Terminal->OnFinished, FParallelOperation->MainOperationProgress);
  TFileOperation Operation = (FLAGSET(FParallelOperation->Params, cpDelete) ? foMove : foCopy);
  bool Temp = FLAGSET(FParallelOperation->Params, cpTemporary);

  OperationProgress.Start(
    // CPS limit inherited from parent OperationProgress.
    // Count not known and won't be needed as we will always have TotalSize as  we always transfer a single file at a time.
    Operation, FParallelOperation->Side, -1, Temp, FParallelOperation->TargetDir, 0, odoIdle);

  try
  {
    bool Continue = true;
    do
    {
      int GotNext = Terminal->CopyToParallel(FParallelOperation, &OperationProgress);
      if (GotNext < 0)
      {
        Continue = false;
      }
      else if (!FQueue->ContinueParallelOperation())
      {
        Continue = false;
      }
    }
    while (Continue);
  }
  __finally
  {
    OperationProgress.Stop();
    FParallelOperation->RemoveClient();
  }
}
//---------------------------------------------------------------------------
// TDownloadQueueItem
//---------------------------------------------------------------------------
static void ExtractRemoteSourcePath(TTerminal * Terminal, TStrings * Files, UnicodeString & Path)
{
  if (!UnixExtractCommonPath(Files, Path))
  {
    Path = Terminal->CurrentDirectory;
  }
  Path = UnixExcludeTrailingBackslash(Path);
}
//---------------------------------------------------------------------------
static bool IsSingleFileDownload(TStrings * FilesToCopy)
{
  return
    (FilesToCopy->Count == 1) &&
    !static_cast<TRemoteFile *>(FilesToCopy->Objects[0])->IsDirectory;
}
//---------------------------------------------------------------------------
__fastcall TDownloadQueueItem::TDownloadQueueItem(TTerminal * Terminal,
    TStrings * FilesToCopy, const UnicodeString & TargetDir,
    const TCopyParamType * CopyParam, int Params, bool Parallel) :
  TTransferQueueItem(
    Terminal, FilesToCopy, TargetDir, CopyParam, Params, osRemote, IsSingleFileDownload(FilesToCopy), Parallel)
{
  if (FilesToCopy->Count > 1)
  {
    ExtractRemoteSourcePath(Terminal, FilesToCopy, FInfo->Source);
    FInfo->ModifiedRemote = FLAGCLEAR(Params, cpDelete) ? UnicodeString() :
      UnixIncludeTrailingBackslash(FInfo->Source);
  }
  else
  {
    DebugAssert(FilesToCopy->Count > 0);
    FInfo->Source = FilesToCopy->Strings[0];
    if (UnixExtractFilePath(FInfo->Source).IsEmpty())
    {
      FInfo->Source = UnixIncludeTrailingBackslash(Terminal->CurrentDirectory) +
        FInfo->Source;
      FInfo->ModifiedRemote = FLAGCLEAR(Params, cpDelete) ? UnicodeString() :
        UnixIncludeTrailingBackslash(Terminal->CurrentDirectory);
    }
    else
    {
      FInfo->ModifiedRemote = FLAGCLEAR(Params, cpDelete) ? UnicodeString() :
        UnixExtractFilePath(FInfo->Source);
    }
  }

  if (FLAGSET(Params, cpTemporary))
  {
    FInfo->Destination = L"";
  }
  else
  {
    FInfo->Destination =
      IncludeTrailingBackslash(TargetDir) + CopyParam->FileMask;
  }
  FInfo->ModifiedLocal = IncludeTrailingBackslash(TargetDir);
}
//---------------------------------------------------------------------------
void __fastcall TDownloadQueueItem::DoTransferExecute(TTerminal * Terminal, TParallelOperation * ParallelOperation)
{
  Terminal->CopyToLocal(FFilesToCopy, FTargetDir, FCopyParam, FParams, ParallelOperation);
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
TRemoteDeleteQueueItem::TRemoteDeleteQueueItem(TTerminal * Terminal, TStrings * FilesToDelete, int Params) :
  TLocatedQueueItem(Terminal)
{
  FInfo->Operation = foDelete;
  FInfo->Side = osRemote;

  DebugAssert(FilesToDelete != NULL);
  FFilesToDelete.reset(TRemoteFileList::CloneStrings(FilesToDelete));
  ExtractRemoteSourcePath(Terminal, FilesToDelete, FInfo->Source);

  FInfo->ModifiedRemote = FInfo->Source;

  FParams = Params;
}
//---------------------------------------------------------------------------
void __fastcall TRemoteDeleteQueueItem::DoExecute(TTerminal * Terminal)
{
  TLocatedQueueItem::DoExecute(Terminal);

  DebugAssert(Terminal != NULL);
  Terminal->DeleteFiles(FFilesToDelete.get(), FParams);
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
TLocalDeleteQueueItem::TLocalDeleteQueueItem(TStrings * FilesToDelete, int Params) :
  TQueueItem()
{
  FInfo->Operation = foDelete;
  FInfo->Side = osLocal;

  DebugAssert(FilesToDelete != NULL);
  FFilesToDelete.reset(CloneStrings(FilesToDelete));
  ExtractLocalSourcePath(FilesToDelete, FInfo->Source);

  FInfo->ModifiedLocal = FInfo->Source;

  FParams = Params;
}
//---------------------------------------------------------------------------
void __fastcall TLocalDeleteQueueItem::DoExecute(TTerminal * Terminal)
{
  DebugAssert(Terminal != NULL);
  Terminal->DeleteLocalFiles(FFilesToDelete.get(), FParams);
}
//---------------------------------------------------------------------------
// TTerminalThread
//---------------------------------------------------------------------------
__fastcall TTerminalThread::TTerminalThread(TTerminal * Terminal) :
  TSignalThread(false), FTerminal(Terminal)
{
  FAction = NULL;
  FActionEvent = CreateEvent(NULL, false, false, NULL);
  FException = NULL;
  FIdleException = NULL;
  FOnIdle = NULL;
  FUserAction = NULL;
  FCancel = false;
  FAbandoned = false;
  FAllowAbandon = false;
  FMainThread = GetCurrentThreadId();
  FSection = new TCriticalSection();

  FOnInformation = FTerminal->OnInformation;
  FOnQueryUser = FTerminal->OnQueryUser;
  FOnPromptUser = FTerminal->OnPromptUser;
  FOnShowExtendedException = FTerminal->OnShowExtendedException;
  FOnDisplayBanner = FTerminal->OnDisplayBanner;
  FOnChangeDirectory = FTerminal->OnChangeDirectory;
  FOnReadDirectory = FTerminal->OnReadDirectory;
  FOnStartReadDirectory = FTerminal->OnStartReadDirectory;
  FOnReadDirectoryProgress = FTerminal->OnReadDirectoryProgress;
  FOnInitializeLog = FTerminal->OnInitializeLog;

  FTerminal->OnInformation = TerminalInformation;
  FTerminal->OnQueryUser = TerminalQueryUser;
  FTerminal->OnPromptUser = TerminalPromptUser;
  FTerminal->OnShowExtendedException = TerminalShowExtendedException;
  FTerminal->OnDisplayBanner = TerminalDisplayBanner;
  FTerminal->OnChangeDirectory = TerminalChangeDirectory;
  FTerminal->OnReadDirectory = TerminalReadDirectory;
  FTerminal->OnStartReadDirectory = TerminalStartReadDirectory;
  FTerminal->OnReadDirectoryProgress = TerminalReadDirectoryProgress;
  FTerminal->OnInitializeLog = TerminalInitializeLog;

  Start();
}
//---------------------------------------------------------------------------
__fastcall TTerminalThread::~TTerminalThread()
{
  Close();

  CloseHandle(FActionEvent);

  DebugAssert(FTerminal->OnInformation == TerminalInformation);
  DebugAssert(FTerminal->OnQueryUser == TerminalQueryUser);
  DebugAssert(FTerminal->OnPromptUser == TerminalPromptUser);
  DebugAssert(FTerminal->OnShowExtendedException == TerminalShowExtendedException);
  DebugAssert(FTerminal->OnDisplayBanner == TerminalDisplayBanner);
  DebugAssert(FTerminal->OnChangeDirectory == TerminalChangeDirectory);
  DebugAssert(FTerminal->OnReadDirectory == TerminalReadDirectory);
  DebugAssert(FTerminal->OnStartReadDirectory == TerminalStartReadDirectory);
  DebugAssert(FTerminal->OnReadDirectoryProgress == TerminalReadDirectoryProgress);
  DebugAssert(FTerminal->OnInitializeLog == TerminalInitializeLog);

  FTerminal->OnInformation = FOnInformation;
  FTerminal->OnQueryUser = FOnQueryUser;
  FTerminal->OnPromptUser = FOnPromptUser;
  FTerminal->OnShowExtendedException = FOnShowExtendedException;
  FTerminal->OnDisplayBanner = FOnDisplayBanner;
  FTerminal->OnChangeDirectory = FOnChangeDirectory;
  FTerminal->OnReadDirectory = FOnReadDirectory;
  FTerminal->OnStartReadDirectory = FOnStartReadDirectory;
  FTerminal->OnReadDirectoryProgress = FOnReadDirectoryProgress;
  FTerminal->OnInitializeLog = FOnInitializeLog;

  delete FSection;
  if (FAbandoned)
  {
    delete FTerminal;
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalThread::Cancel()
{
  FCancel = true;
  FCancelAfter = IncMilliSecond(Now(), 1000);
}
//---------------------------------------------------------------------------
void __fastcall TTerminalThread::Idle()
{
  // If user action is needed during Idle() call from TTerminalThread::WaitForUserAction
  // (e.g. when disconnect is detected and session get reconnected)
  // unconditional Enter() here would deadlock.
  if (FSection->TryEnter())
  {
    try
    {
      // only when running user action already,
      // so that the exception is caught, saved and actually
      // passed back into the terminal thread, saved again
      // and passed back to us
      if ((FUserAction != NULL) && (FIdleException != NULL))
      {
        Rethrow(FIdleException);
      }
      FPendingIdle = true;
    }
    __finally
    {
      FSection->Release();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalThread::TerminalOpen()
{
  RunAction(TerminalOpenEvent);
}
//---------------------------------------------------------------------------
void __fastcall TTerminalThread::TerminalReopen()
{
  RunAction(TerminalReopenEvent);
}
//---------------------------------------------------------------------------
void TTerminalThread::DiscardException()
{
  SAFE_DESTROY(FException);
}
//---------------------------------------------------------------------------
void __fastcall TTerminalThread::RunAction(TNotifyEvent Action)
{
  DebugAssert(FAction == NULL);
  DebugAssert(FException == NULL);
  DebugAssert(FIdleException == NULL);
  DebugAssert(FOnIdle != NULL);

  FCancelled = false;
  FAction = Action;
  try
  {
    try
    {
      TriggerEvent();

      bool Done = false;
      const unsigned int MaxWait = 50;
      unsigned int Wait = MaxWait;

      do
      {
        switch (WaitForSingleObject(FActionEvent, Wait))
        {
          case WAIT_OBJECT_0:
            Done = true;
            break;

          case WAIT_TIMEOUT:
            if (FUserAction != NULL)
            {
              try
              {
                FUserAction->Execute(NULL);
              }
              catch (Exception & E)
              {
                SaveException(E, FException);
              }

              FUserAction = NULL;
              TriggerEvent();
              Wait = 0;
            }
            else
            {
              if (FOnIdle != NULL)
              {
                FOnIdle(NULL);
              }
              Wait = std::min(Wait + 10, MaxWait);
            }
            break;

          default:
            throw Exception(L"Error waiting for background session task to complete");
        }

        if (AllowAbandon && !Done && FCancel && (Now() >= FCancelAfter))
        {
          TGuard Guard(FSection);
          if (WaitForSingleObject(FActionEvent, 0) != WAIT_OBJECT_0)
          {
            FAbandoned = true;
            FCancelled = true;
            FatalAbort();
          }
        }
      }
      while (!Done);


      if (Done)
      {
        Rethrow(FException);
      }
    }
    __finally
    {
      FAction = NULL;
      DiscardException();
    }
  }
  catch(...)
  {
    if (FCancelled)
    {
      // even if the abort thrown as result of Cancel() was wrapped into
      // some higher-level exception, normalize back to message-less fatal
      // exception here
      FatalAbort();
    }
    else
    {
      throw;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalThread::TerminalOpenEvent(TObject * /*Sender*/)
{
  FTerminal->Open();
}
//---------------------------------------------------------------------------
void __fastcall TTerminalThread::TerminalReopenEvent(TObject * /*Sender*/)
{
  FTerminal->Reopen(0);
}
//---------------------------------------------------------------------------
void __fastcall TTerminalThread::ProcessEvent()
{
  DebugAssert(FEvent != NULL);
  DebugAssert(FException == NULL);

  // Needed at least for TXMLDocument use in TS3FileSystem
  CoInitialize(NULL);

  try
  {
    FAction(NULL);
  }
  catch(Exception & E)
  {
    SaveException(E, FException);
  }

  {
    TGuard Guard(FSection);
    if (!FAbandoned)
    {
      SetEvent(FActionEvent);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalThread::Rethrow(Exception *& Exception)
{
  if (Exception != NULL)
  {
    try
    {
      RethrowException(Exception);
    }
    __finally
    {
      SAFE_DESTROY(Exception);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalThread::SaveException(Exception & E, Exception *& Exception)
{
  DebugAssert(Exception == NULL);

  Exception = CloneException(&E);
}
//---------------------------------------------------------------------------
void __fastcall TTerminalThread::FatalAbort()
{
  if (FAbandoned)
  {
    // We cannot use TTerminal::FatalError as the terminal still runs on a background thread,
    // may have its TCallbackGuard armed right now.
    throw ESshFatal(NULL, L"");
  }
  else
  {
    FTerminal->FatalAbort();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalThread::CheckCancel()
{
  if (FCancel && !FCancelled)
  {
    FCancelled = true;
    FatalAbort();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalThread::WaitForUserAction(TUserAction * UserAction)
{
  DWORD Thread = GetCurrentThreadId();
  // we can get called from the main thread from within Idle,
  // should be only to call HandleExtendedException
  if (Thread == FMainThread)
  {
    if (UserAction != NULL)
    {
      UserAction->Execute(NULL);
    }
  }
  else
  {
    // we should be called from our thread only,
    // with exception noted above
    DebugAssert(Thread == FThreadId);

    bool DoCheckCancel =
      DebugAlwaysFalse(UserAction == NULL) || !UserAction->Force() || FAbandoned;
    if (DoCheckCancel)
    {
      CheckCancel();
    }

    // have to save it as we can go recursive via TQueryParams::TimerEvent,
    // see TTerminalThread::TerminalQueryUser
    TUserAction * PrevUserAction = FUserAction;
    try
    {
      FUserAction = UserAction;

      while (true)
      {

        {
          TGuard Guard(FSection);
          // If idle exception is already set, we are only waiting
          // for the main thread to pick it up
          // (or at least to finish handling the user action, so
          // that we rethrow the idle exception below)
          // Also if idle exception is set, it is probable that terminal
          // is not active anyway.
          if (FTerminal->Active && FPendingIdle && (FIdleException == NULL))
          {
            FPendingIdle = false;
            try
            {
              FTerminal->Idle();
            }
            catch (Exception & E)
            {
              SaveException(E, FIdleException);
            }
          }
        }

        int WaitResult = WaitForEvent(1000);
        if (WaitResult == 0)
        {
          SAFE_DESTROY(FIdleException);
          FatalAbort();
        }
        else if (WaitResult > 0)
        {
          break;
        }
      }


      Rethrow(FException);

      if (FIdleException != NULL)
      {
        // idle exception was not used to cancel the user action
        // (if it where it would be already cloned into the FException above
        // and rethrown)
        Rethrow(FIdleException);
      }
    }
    __finally
    {
      FUserAction = PrevUserAction;
      DiscardException();
    }

    // Contrary to a call before, this is unconditional,
    // otherwise cancelling authentication won't work,
    // if it is tried only after the last user action
    // (what is common, when cancelling while waiting for
    // resolving of unresolvable host name, where the last user action is
    // "resolving hostname" information action)
    CheckCancel();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalThread::TerminalInformation(
  TTerminal * Terminal, const UnicodeString & Str, int Phase, const UnicodeString & Additional)
{
  TInformationUserAction Action(FOnInformation);
  Action.Terminal = Terminal;
  Action.Str = Str;
  Action.Phase = Phase;
  Action.Additional = Additional;

  WaitForUserAction(&Action);
}
//---------------------------------------------------------------------------
void __fastcall TTerminalThread::TerminalQueryUser(TObject * Sender,
  const UnicodeString Query, TStrings * MoreMessages, unsigned int Answers,
  const TQueryParams * Params, unsigned int & Answer, TQueryType Type, void * Arg)
{
  DebugUsedParam(Arg);
  DebugAssert(Arg == NULL);

  // note about TQueryParams::TimerEvent
  // So far there is only one use for this, the TSecureShell::SendBuffer,
  // which should be thread-safe, as long as the terminal thread,
  // is stopped waiting for OnQueryUser to finish.

  // note about TQueryButtonAlias::OnClick
  // So far there is only one use for this, the TClipboardHandler,
  // which is thread-safe.

  TQueryUserAction Action(FOnQueryUser);
  Action.Sender = Sender;
  Action.Query = Query;
  Action.MoreMessages = MoreMessages;
  Action.Answers = Answers;
  Action.Params = Params;
  Action.Answer = Answer;
  Action.Type = Type;

  WaitForUserAction(&Action);

  Answer = Action.Answer;
}
//---------------------------------------------------------------------------
void __fastcall TTerminalThread::TerminalInitializeLog(TObject * Sender)
{
  if (FOnInitializeLog != NULL)
  {
    // never used, so not tested either
    DebugFail();
    TNotifyAction Action(FOnInitializeLog);
    Action.Sender = Sender;

    WaitForUserAction(&Action);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalThread::TerminalPromptUser(TTerminal * Terminal,
  TPromptKind Kind, UnicodeString Name, UnicodeString Instructions, TStrings * Prompts,
  TStrings * Results, bool & Result, void * Arg)
{
  DebugUsedParam(Arg);
  DebugAssert(Arg == NULL);

  TPromptUserAction Action(FOnPromptUser);
  Action.Terminal = Terminal;
  Action.Kind = Kind;
  Action.Name = Name;
  Action.Instructions = Instructions;
  Action.Prompts = Prompts;
  Action.Results->AddStrings(Results);

  WaitForUserAction(&Action);

  Results->Clear();
  Results->AddStrings(Action.Results);
  Result = Action.Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminalThread::TerminalShowExtendedException(
  TTerminal * Terminal, Exception * E, void * Arg)
{
  DebugUsedParam(Arg);
  DebugAssert(Arg == NULL);

  TShowExtendedExceptionAction Action(FOnShowExtendedException);
  Action.Terminal = Terminal;
  Action.E = E;

  WaitForUserAction(&Action);
}
//---------------------------------------------------------------------------
void __fastcall TTerminalThread::TerminalDisplayBanner(TTerminal * Terminal,
  UnicodeString SessionName, const UnicodeString & Banner,
  bool & NeverShowAgain, int Options, unsigned int & Params)
{
  TDisplayBannerAction Action(FOnDisplayBanner);
  Action.Terminal = Terminal;
  Action.SessionName = SessionName;
  Action.Banner = Banner;
  Action.NeverShowAgain = NeverShowAgain;
  Action.Options = Options;
  Action.Params = Params;

  WaitForUserAction(&Action);

  NeverShowAgain = Action.NeverShowAgain;
  Params = Action.Params;
}
//---------------------------------------------------------------------------
void __fastcall TTerminalThread::TerminalChangeDirectory(TObject * Sender)
{
  TNotifyAction Action(FOnChangeDirectory);
  Action.Sender = Sender;

  WaitForUserAction(&Action);
}
//---------------------------------------------------------------------------
void __fastcall TTerminalThread::TerminalReadDirectory(TObject * Sender, Boolean ReloadOnly)
{
  TReadDirectoryAction Action(FOnReadDirectory);
  Action.Sender = Sender;
  Action.ReloadOnly = ReloadOnly;

  WaitForUserAction(&Action);
}
//---------------------------------------------------------------------------
void __fastcall TTerminalThread::TerminalStartReadDirectory(TObject * Sender)
{
  TNotifyAction Action(FOnStartReadDirectory);
  Action.Sender = Sender;

  WaitForUserAction(&Action);
}
//---------------------------------------------------------------------------
void __fastcall TTerminalThread::TerminalReadDirectoryProgress(
  TObject * Sender, int Progress, int ResolvedLinks, bool & Cancel)
{
  TReadDirectoryProgressAction Action(FOnReadDirectoryProgress);
  Action.Sender = Sender;
  Action.Progress = Progress;
  Action.ResolvedLinks = ResolvedLinks;
  Action.Cancel = Cancel;

  WaitForUserAction(&Action);

  Cancel = Action.Cancel;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminalThread::Release()
{
  bool Result = !FAbandoned;
  if (Result)
  {
    delete this;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminalThread::Finished()
{
  return TSimpleThread::Finished() || FAbandoned;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
TQueueFileList::TQueueFileList() :
  FList(new TStringList())
{
  Clear();
}
//---------------------------------------------------------------------------
void TQueueFileList::Clear()
{
  FList->Clear();
  FLastParallelOperation = NULL;
  FLastParallelOperationVersion = -1;
}
//---------------------------------------------------------------------------
void TQueueFileList::Add(const UnicodeString & FileName, int State)
{
  FList->AddObject(FileName, reinterpret_cast<TObject *>(State));
}
//---------------------------------------------------------------------------
UnicodeString TQueueFileList::GetFileName(int Index) const
{
  return FList->Strings[Index];
}
//---------------------------------------------------------------------------
int TQueueFileList::GetState(int Index) const
{
  return reinterpret_cast<int>(FList->Objects[Index]);
}
//---------------------------------------------------------------------------
void TQueueFileList::SetState(int Index, int State)
{
  FList->Objects[Index] = reinterpret_cast<TObject *>(State);
}
//---------------------------------------------------------------------------
int TQueueFileList::GetCount() const
{
  return FList->Count;
}
