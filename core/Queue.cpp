//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Common.h"
#include "Terminal.h"
#include "Queue.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
class TTerminalItem : public TSignalThread
{
friend class TQueueItem;

public:
  __fastcall TTerminalItem(TTerminalQueue * Queue);
  __fastcall ~TTerminalItem();

  void __fastcall Process(TQueueItem * Item);
  void __fastcall ProcessUserAction();
  void __fastcall Cancel();

protected:
  struct TQueryUserRec
  {
    TObject * Sender;
    AnsiString Query;
    TStrings * MoreMessages;
    int Answers;
    int Params;
    int Answer;
    TQueryType Type;
  };

  struct TPromptUserRec
  {
    TSecureShell * SecureShell;
    AnsiString Prompt;
    TPromptKind Kind;
    AnsiString Response;
    bool Result;
  };

  struct TShowExtendedExceptionRec
  {
    TSecureShell * SecureShell;
    Exception * E;
  };

  TTerminalQueue * FQueue;
  TTerminal * FTerminal;
  TQueueItem * FItem;
  TCriticalSection * FCriticalSection;
  void * FUserActionParams;
  bool FCancel;

  virtual void __fastcall ProcessEvent();
  virtual void __fastcall Finished();
  bool __fastcall WaitForUserAction(TQueueItem::TStatus ItemStatus, void * Params);

  void __fastcall TerminalClose(TObject * Sender);
  void __fastcall TerminalQueryUser(TObject * Sender,
    const AnsiString Query, TStrings * MoreMessages, int Answers,
    int Params, int & Answer, TQueryType Type);
  void __fastcall TerminalPromptUser(TSecureShell * SecureShell,
    AnsiString Prompt, TPromptKind Kind, AnsiString & Response, bool & Result);
  void __fastcall TerminalShowExtendedException(TSecureShell * SecureShell,
    Exception * E);
  void __fastcall OperationFinished(TFileOperation Operation, TOperationSide Side,
    bool DragDrop, const AnsiString FileName, bool Success,
    bool & DisconnectWhenFinished);
  void __fastcall OperationProgress(TFileOperationProgressType & ProgressData,
    TCancelStatus & Cancel);
};
//---------------------------------------------------------------------------
typedef int __fastcall (__closure *TDirectoryAffected)
  (TTerminal * Terminal, const AnsiString Directory);
//---------------------------------------------------------------------------
class TSecondaryTerminal : public TTerminal
{
public:
  __fastcall TSecondaryTerminal(TTerminal * MainTerminal);

  __property TDirectoryAffected OnDirectoryAffected = { read = FOnDirectoryAffected,
    write = FOnDirectoryAffected };

protected:
  virtual void __fastcall DirectoryLoaded(TRemoteFileList * FileList);
  virtual void __fastcall DirectoryModified(const AnsiString Path,
    bool SubDirs);

private:
  TTerminal * FMainTerminal;
  TDirectoryAffected FOnDirectoryAffected;

  void __fastcall DoDirectoryAffected(const AnsiString Path);
};
//---------------------------------------------------------------------------
// TSignalThread
//---------------------------------------------------------------------------
int __fastcall ThreadProc(void * Thread)
{
  TSignalThread * SimpleThread = reinterpret_cast<TSignalThread*>(Thread);
  assert(SimpleThread != NULL);
  try
  {
    SimpleThread->Execute();
  }
  __finally
  {
    SimpleThread->FFinished = true;
    SimpleThread->Finished();
    EndThread(0);
  }
  return 0;
}
//---------------------------------------------------------------------------
__fastcall TSignalThread::TSignalThread() :
  FTerminated(true), FFinished(true), FThread(NULL), FEvent(NULL)
{
  FEvent = CreateEvent(NULL, false, false, NULL);
  assert(FEvent != NULL);

  unsigned ThreadID;
  FThread = reinterpret_cast<HANDLE>(
    BeginThread(NULL, 0, ThreadProc, this, CREATE_SUSPENDED, ThreadID));
  ::SetThreadPriority(FThread, THREAD_PRIORITY_BELOW_NORMAL); 
}
//---------------------------------------------------------------------------
__fastcall TSignalThread::~TSignalThread()
{
  Close();

  if (FThread != NULL)
  {
    CloseHandle(FThread);
  }

  if (FEvent)
  {
    CloseHandle(FEvent);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSignalThread::Start()
{
  FTerminated = false;
  if (ResumeThread(FThread) == 1)
  {
    FFinished = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSignalThread::Finished()
{
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
void __fastcall TSignalThread::Close()
{
  if (!FFinished)
  {
    Terminate();
    WaitFor();
  }
}
//---------------------------------------------------------------------------
void __fastcall TSignalThread::Terminate()
{
  FTerminated = true;
  TriggerEvent();
}
//---------------------------------------------------------------------------
void __fastcall TSignalThread::WaitFor()
{
  WaitForSingleObject(FThread, INFINITE);
}
//---------------------------------------------------------------------------
// TTerminalQueue
//---------------------------------------------------------------------------
__fastcall TTerminalQueue::TTerminalQueue(TTerminal * Terminal,
  TConfiguration * Configuration) :
  FTerminal(Terminal), FTransfersLimit(2),
  FConfiguration(Configuration), FSessionData(NULL), FItems(NULL),
  FTerminals(NULL), FItemsSection(NULL), FFreeTerminals(0),
  FItemsInProcess(0), FTemporaryTerminals(0)
{
  FOnQueryUser = NULL;
  FOnPromptUser = NULL;
  FOnShowExtendedException = NULL;
  FOnQueueItemUpdate = NULL;
  FOnListUpdate = NULL;

  assert(Terminal != NULL);
  FSessionData = new TSessionData("");
  FSessionData->Assign(Terminal->SessionData);
  FSessionData->NonPersistant();

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
  }

  delete FItemsSection;
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
      
      if (Index >= FTransfersLimit)
      {
        assert(FTemporaryTerminals > 0);
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

      Result = (Index < FTransfersLimit);
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
    {
      TGuard Guard(FItemsSection);

      int Index = FItems->Remove(Item);
      assert(Index < FItemsInProcess);
      FItemsInProcess--;
      delete Item;
    }

    DoListUpdate();
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
  TGuard Guard(FItemsSection);

  bool Result = (FItems->IndexOf(Item) >= 0);
  if (Result)
  {
    Item->GetData(Proxy);
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminalQueue::ItemProcessUserAction(TQueueItem * Item)
{
  bool Result;
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
    TerminalItem->ProcessUserAction();
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminalQueue::ItemMove(TQueueItem * Item, TQueueItem * BeforeItem)
{
  bool Result;
  
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

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminalQueue::ItemExecuteNow(TQueueItem * Item)
{
  bool Result;

  {
    TGuard Guard(FItemsSection);

    int Index = FItems->IndexOf(Item);
    Result = (Index >= 0) && (Item->GetStatus() == TQueueItem::qsPending);
    if (Result)
    {
      if (Index != FItemsInProcess)
      {
        assert(Index > FItemsInProcess);
        FItems->Move(Index, FItemsInProcess);
      }

      if (FTerminals->Count >= FTransfersLimit)
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

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminalQueue::ItemDelete(TQueueItem * Item)
{
  bool Result;
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

    if (FItems->Count > FItemsInProcess)
    {
      TGuard Guard(FItemsSection);

      if ((FFreeTerminals == 0) &&
          (FTerminals->Count < FTransfersLimit + FTemporaryTerminals))
      {
        TerminalItem = new TTerminalItem(this);
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
void __fastcall TTerminalQueue::DoQueryUser(TObject * Sender,
  const AnsiString Query, TStrings * MoreMessages, int Answers, int Params,
  int & Answer, TQueryType Type)
{
  if (OnQueryUser != NULL)
  {
    OnQueryUser(Sender, Query, MoreMessages, Answers, Params, Answer, Type);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalQueue::DoPromptUser(TSecureShell * SecureShell,
  AnsiString Prompt, TPromptKind Kind, AnsiString & Response, bool & Result)
{
  if (OnPromptUser != NULL)
  {
    OnPromptUser(SecureShell, Prompt, Kind, Response, Result);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalQueue::DoShowExtendedException(
  TSecureShell * SecureShell, Exception * E)
{
  if (OnShowExtendedException != NULL)
  {
    OnShowExtendedException(SecureShell, E);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalQueue::SetTransfersLimit(int value)
{
  if (FTransfersLimit != value)
  {
    {
      TGuard Guard(FItemsSection);

      if (value < FItemsInProcess)
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
// TSecondaryTerminal
//---------------------------------------------------------------------------
__fastcall TSecondaryTerminal::TSecondaryTerminal(TTerminal * MainTerminal) :
  TTerminal(), FMainTerminal(MainTerminal)
{
  assert(FMainTerminal != NULL);
  OnDirectoryAffected = NULL;
  UseBusyCursor = false;
}
//---------------------------------------------------------------------------
void __fastcall TSecondaryTerminal::DirectoryLoaded(TRemoteFileList * FileList)
{
  FMainTerminal->DirectoryLoaded(FileList);
  assert(FileList != NULL);
}
//---------------------------------------------------------------------------
void __fastcall TSecondaryTerminal::DirectoryModified(const AnsiString Path,
  bool SubDirs)
{
  // clear cache of main terminal
  FMainTerminal->DirectoryModified(Path, SubDirs);
}
//---------------------------------------------------------------------------
// TTerminalItem
//---------------------------------------------------------------------------
__fastcall TTerminalItem::TTerminalItem(TTerminalQueue * Queue) :
  TSignalThread(), FQueue(Queue), FTerminal(NULL), FItem(NULL),
  FCriticalSection(NULL), FUserActionParams(NULL)
{
  FCriticalSection = new TCriticalSection();

  FTerminal = new TSecondaryTerminal(FQueue->FTerminal);
  try
  {
    FTerminal->Configuration = Queue->FConfiguration;
    FTerminal->SessionData = Queue->FSessionData;

    FTerminal->OnClose = TerminalClose;
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
  FItem->FTerminalItem = this;

  try
  {
    assert(FItem != NULL);

    if (!FTerminal->Active)
    {
      FItem->SetStatus(TQueueItem::qsConnecting);

      FTerminal->Open();
      FTerminal->DoStartup();
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
    FTerminal->DoShowExtendedException(&E);
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
void __fastcall TTerminalItem::Cancel()
{
  FCancel = true;
}
//---------------------------------------------------------------------------
void __fastcall TTerminalItem::ProcessUserAction()
{
  assert(FItem != NULL);
  assert(FUserActionParams != NULL);

  if (FItem->GetStatus() == TQueueItem::qsQuery)
  {
    TQueryUserRec * Params;
    Params = reinterpret_cast<TQueryUserRec *>(FUserActionParams);

    FQueue->DoQueryUser(Params->Sender, Params->Query, Params->MoreMessages,
      Params->Answers, Params->Params, Params->Answer, Params->Type);
  }
  else if (FItem->GetStatus() == TQueueItem::qsPrompt)
  {
    TPromptUserRec * Params;
    Params = reinterpret_cast<TPromptUserRec *>(FUserActionParams);

    FQueue->DoPromptUser(Params->SecureShell, Params->Prompt,
      Params->Kind, Params->Response, Params->Result);
  }
  else if (FItem->GetStatus() == TQueueItem::qsError)
  {
    TShowExtendedExceptionRec * Params;
    Params = reinterpret_cast<TShowExtendedExceptionRec *>(FUserActionParams);

    FQueue->DoShowExtendedException(Params->SecureShell, Params->E);
  }
  else
  {
    assert(false);
  }

  TriggerEvent();
}
//---------------------------------------------------------------------------
bool __fastcall TTerminalItem::WaitForUserAction(
  TQueueItem::TStatus ItemStatus, void * Params)
{
  assert(FItem != NULL);
  assert((FItem->GetStatus() == TQueueItem::qsProcessing) ||
    (FItem->GetStatus() == TQueueItem::qsConnecting));

  bool Result;

  TQueueItem::TStatus PrevStatus = FItem->GetStatus();

  try
  {
    FUserActionParams = Params;

    FItem->SetStatus(ItemStatus);

    Result = !FTerminated && WaitForEvent() && !FCancel;
  }
  __finally
  {
    FUserActionParams = NULL;
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
void __fastcall TTerminalItem::TerminalClose(TObject * /*Sender*/)
{
  // nothing
}
//---------------------------------------------------------------------------
void __fastcall TTerminalItem::TerminalQueryUser(TObject * Sender,
  const AnsiString Query, TStrings * MoreMessages, int Answers,
  int Params, int & Answer, TQueryType Type)
{
  TQueryUserRec QueryUserRec;
  QueryUserRec.Sender = Sender;
  QueryUserRec.Query = Query;
  QueryUserRec.MoreMessages = MoreMessages;
  QueryUserRec.Answers = Answers;
  QueryUserRec.Params = Params;
  QueryUserRec.Answer = Answer;
  QueryUserRec.Type = Type;

  if (WaitForUserAction(TQueueItem::qsQuery, &QueryUserRec))
  {
    Answer = QueryUserRec.Answer;
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalItem::TerminalPromptUser(TSecureShell * SecureShell,
  AnsiString Prompt, TPromptKind Kind, AnsiString & Response, bool & Result)
{
  TPromptUserRec PromptUserRec;
  PromptUserRec.SecureShell = SecureShell;
  PromptUserRec.Prompt = Prompt;
  PromptUserRec.Kind = Kind;
  PromptUserRec.Response = Response;
  PromptUserRec.Result = Result;

  if (WaitForUserAction(TQueueItem::qsPrompt, &PromptUserRec))
  {
    Response = PromptUserRec.Response;
    Result = PromptUserRec.Result;
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalItem::TerminalShowExtendedException(
  TSecureShell * SecureShell, Exception * E)
{
  if (!E->Message.IsEmpty() &&
      (dynamic_cast<EAbort*>(E) == NULL))
  {
    TShowExtendedExceptionRec ShowExtendedExceptionRec;
    ShowExtendedExceptionRec.SecureShell = SecureShell;
    ShowExtendedExceptionRec.E = E;

    WaitForUserAction(TQueueItem::qsError, &ShowExtendedExceptionRec);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalItem::OperationFinished(TFileOperation /*Operation*/,
  TOperationSide /*Side*/, bool /*DragDrop*/, const AnsiString /*FileName*/,
  bool /*Success*/, bool & /*DisconnectWhenFinished*/)
{
  // nothing
}
//---------------------------------------------------------------------------
void __fastcall TTerminalItem::OperationProgress(
  TFileOperationProgressType & ProgressData, TCancelStatus & Cancel)
{
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
// TQueueItem
//---------------------------------------------------------------------------
__fastcall TQueueItem::TQueueItem() :
  FStatus(qsPending), FTerminalItem(NULL), FSection(NULL), FProgressData(NULL),
  FQueue(NULL), FInfo(NULL)
{
  FSection = new TCriticalSection();
  FInfo = new TInfo();
}
//---------------------------------------------------------------------------
__fastcall TQueueItem::~TQueueItem()
{
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
  const TFileOperationProgressType & ProgressData)
{
  {
    TGuard Guard(FSection);

    assert(FProgressData != NULL);
    *FProgressData = ProgressData;
    FProgressData->Reset();
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
// TQueueItemProxy
//---------------------------------------------------------------------------
__fastcall TQueueItemProxy::TQueueItemProxy(TTerminalQueue * Queue,
  TQueueItem * QueueItem) :
  FQueue(Queue), FQueueItem(QueueItem), FProgressData(NULL),
  FQueueStatus(NULL), FInfo(NULL), FFlag(false),
  FProcessingUserAction(false)
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
bool __fastcall TQueueItemProxy::ProcessUserAction()
{
  assert(FQueueItem != NULL);

  bool Result;
  FProcessingUserAction = true;
  try
  {
    Result = FQueue->ItemProcessUserAction(FQueueItem);
  }
  __finally
  {
    FProcessingUserAction = false;
  }
  return Result;
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
    ExtractCommonPath(FilesToCopy, FInfo->Source);
    FInfo->Source = ExcludeTrailingBackslash(FInfo->Source);
  }
  else
  {
    assert(FilesToCopy->Count > 0);
    FInfo->Source = FilesToCopy->Strings[0];
  }

  FInfo->Destination =
    UnixIncludeTrailingBackslash(TargetDir) + CopyParam->FileMask;
  FInfo->ModifiesLocal = ((Params & cpDelete) != 0);
  FInfo->ModifiesRemote = true;
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
  }
  else
  {
    assert(FilesToCopy->Count > 0);
    FInfo->Source = FilesToCopy->Strings[0];
    if (UnixExtractFilePath(FInfo->Source).IsEmpty())
    {
      FInfo->Source = UnixIncludeTrailingBackslash(Terminal->CurrentDirectory) +
        FInfo->Source;
    }
  }

  FInfo->Destination =
    IncludeTrailingBackslash(TargetDir) + CopyParam->FileMask;
  FInfo->ModifiesLocal = true;
  FInfo->ModifiesRemote = ((Params & cpDelete) != 0);
}
//---------------------------------------------------------------------------
void __fastcall TDownloadQueueItem::DoExecute(TTerminal * Terminal)
{
  TTransferQueueItem::DoExecute(Terminal);

  assert(Terminal != NULL);
  Terminal->CopyToLocal(FFilesToCopy, FTargetDir, FCopyParam, FParams);
}
