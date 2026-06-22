//---------------------------------------------------------------------------
#ifndef QueueH
#define QueueH
//---------------------------------------------------------------------------
#include "Terminal.h"
#include "FileOperationProgress.h"
//---------------------------------------------------------------------------
class TSimpleThread
{
public:
  __fastcall TSimpleThread();
  virtual __fastcall ~TSimpleThread();

  virtual void __fastcall Start();
  void __fastcall WaitFor(unsigned int Milliseconds = INFINITE);
  virtual void __fastcall Terminate() = 0;
  void __fastcall Close();
  bool __fastcall IsFinished();

protected:
  HANDLE FThread;
  TThreadID FThreadId;
  bool FFinished;

  virtual void __fastcall Execute() = 0;
  virtual bool __fastcall Finished();

  static int __fastcall ThreadProc(void * Thread);
};
//---------------------------------------------------------------------------
class TSignalThread : public TSimpleThread
{
public:
  virtual void __fastcall Start();
  virtual void __fastcall Terminate();
  void __fastcall TriggerEvent();

protected:
  HANDLE FEvent;
  bool FTerminated;

  __fastcall TSignalThread(bool LowPriority, HANDLE Event = NULL);
  virtual __fastcall ~TSignalThread();

  virtual bool __fastcall WaitForEvent();
  int __fastcall WaitForEvent(unsigned int Timeout);
  virtual void __fastcall Execute();
  virtual void __fastcall ProcessEvent() = 0;
};
//---------------------------------------------------------------------------
class TTerminal;
class TQueueItem;
class TTerminalQueue;
class TQueueItemProxy;
class TTerminalQueueStatus;
class TQueueFileList;
class TTerminalItem;
//---------------------------------------------------------------------------
typedef void __fastcall (__closure * TQueueListUpdate)
  (TTerminalQueue * Queue);
typedef void __fastcall (__closure * TQueueItemUpdateEvent)
  (TTerminalQueue * Queue, TQueueItem * Item);
enum TQueueEvent { qeEmpty, qeEmptyButMonitored, qePendingUserAction };
typedef void __fastcall (__closure * TQueueEventEvent)
  (TTerminalQueue * Queue, TQueueEvent Event);
//---------------------------------------------------------------------------
class TTerminalQueue : public TSignalThread
{
friend class TQueueItem;
friend class TQueueItemProxy;
friend class TTransferQueueItem;
friend class TParallelTransferQueueItem;

public:
  __fastcall TTerminalQueue(TTerminal * Terminal, TConfiguration * Configuration);
  virtual __fastcall ~TTerminalQueue();

  void __fastcall AddItem(TQueueItem * Item);
  TTerminalQueueStatus * __fastcall CreateStatus(TTerminalQueueStatus * Current);
  void __fastcall Idle();

  __property bool IsEmpty = { read = GetIsEmpty };
  __property int TransfersLimit = { read = FTransfersLimit, write = SetTransfersLimit };
  __property int KeepDoneItemsFor = { read = FKeepDoneItemsFor, write = SetKeepDoneItemsFor };
  __property int ParallelDurationThreshold = { read = GetParallelDurationThreshold };
  __property bool Enabled = { read = FEnabled, write = SetEnabled };
  __property TQueryUserEvent OnQueryUser = { read = FOnQueryUser, write = FOnQueryUser };
  __property TPromptUserEvent OnPromptUser = { read = FOnPromptUser, write = FOnPromptUser };
  __property TExtendedExceptionEvent OnShowExtendedException = { read = FOnShowExtendedException, write = FOnShowExtendedException };
  __property TQueueListUpdate OnListUpdate = { read = FOnListUpdate, write = FOnListUpdate };
  __property TQueueItemUpdateEvent OnQueueItemUpdate = { read = FOnQueueItemUpdate, write = FOnQueueItemUpdate };
  __property TQueueEventEvent OnEvent = { read = FOnEvent, write = FOnEvent };

protected:
  friend class TTerminalItem;
  friend class TQueryUserAction;
  friend class TPromptUserAction;
  friend class TShowExtendedExceptionAction;

  TQueryUserEvent FOnQueryUser;
  TPromptUserEvent FOnPromptUser;
  TExtendedExceptionEvent FOnShowExtendedException;
  TQueueItemUpdateEvent FOnQueueItemUpdate;
  TQueueListUpdate FOnListUpdate;
  TQueueEventEvent FOnEvent;
  TTerminal * FTerminal;
  TConfiguration * FConfiguration;
  TSessionData * FSessionData;
  TList * FItems;
  TList * FDoneItems;
  int FItemsInProcess;
  TCriticalSection * FItemsSection;
  int FFreeTerminals;
  TList * FTerminals;
  TList * FForcedItems;
  int FTemporaryTerminals;
  int FOverallTerminals;
  int FTransfersLimit;
  int FKeepDoneItemsFor;
  bool FEnabled;
  TDateTime FIdleInterval;
  TDateTime FLastIdle;

  inline static TQueueItem * __fastcall GetItem(TList * List, int Index);
  inline TQueueItem * __fastcall GetItem(int Index);
  void __fastcall FreeItemsList(TList * List);
  void __fastcall UpdateStatusForList(
    TTerminalQueueStatus * Status, TList * List, TTerminalQueueStatus * Current);
  bool __fastcall ItemGetData(TQueueItem * Item, TQueueItemProxy * Proxy, TQueueFileList * FileList);
  bool __fastcall ItemProcessUserAction(TQueueItem * Item, void * Arg);
  bool __fastcall ItemMove(TQueueItem * Item, TQueueItem * BeforeItem);
  bool __fastcall ItemExecuteNow(TQueueItem * Item);
  bool __fastcall ItemDelete(TQueueItem * Item);
  bool __fastcall ItemPause(TQueueItem * Item, bool Pause);
  bool __fastcall ItemSetCPSLimit(TQueueItem * Item, unsigned long CPSLimit);
  bool __fastcall ItemGetCPSLimit(TQueueItem * Item, unsigned long & CPSLimit);

  void __fastcall RetryItem(TQueueItem * Item);
  void __fastcall DeleteItem(TQueueItem * Item, bool CanKeep);

  virtual bool __fastcall WaitForEvent();
  virtual void __fastcall ProcessEvent();
  void __fastcall TerminalFinished(TTerminalItem * TerminalItem);
  bool __fastcall TerminalFree(TTerminalItem * TerminalItem);
  int __fastcall GetParallelDurationThreshold();

  void __fastcall DoQueueItemUpdate(TQueueItem * Item);
  void __fastcall DoListUpdate();
  void __fastcall DoEvent(TQueueEvent Event);

  void __fastcall SetTransfersLimit(int value);
  void __fastcall SetKeepDoneItemsFor(int value);
  void __fastcall SetEnabled(bool value);
  bool __fastcall GetIsEmpty();

  bool __fastcall TryAddParallelOperation(TQueueItem * Item, bool Force);
  bool __fastcall ContinueParallelOperation();
};
//---------------------------------------------------------------------------
class TQueueItem
{
friend class TTerminalQueue;
friend class TTerminalItem;
friend class TParallelTransferQueueItem;

public:
  enum TStatus {
    qsPending, qsConnecting, qsProcessing, qsPrompt, qsQuery, qsError,
    qsPaused, qsDone };
  struct TInfo
  {
    TFileOperation Operation;
    TOperationSide Side;
    UnicodeString Source;
    UnicodeString Destination;
    UnicodeString ModifiedLocal;
    UnicodeString ModifiedRemote;
    bool SingleFile;
    bool Primary;
    void * GroupToken;
  };

  static bool __fastcall IsUserActionStatus(TStatus Status);

  __property TStatus Status = { read = GetStatus };
  __property HANDLE CompleteEvent = { read = FCompleteEvent, write = FCompleteEvent };

protected:
  TStatus FStatus;
  TCriticalSection * FSection;
  TTerminalItem * FTerminalItem;
  TFileOperationProgressType * FProgressData;
  TQueueItem::TInfo * FInfo;
  TTerminalQueue * FQueue;
  HANDLE FCompleteEvent;
  long FCPSLimit;
  TDateTime FDoneAt;

  __fastcall TQueueItem();
  virtual __fastcall ~TQueueItem();

  void __fastcall SetStatus(TStatus Status);
  TStatus __fastcall GetStatus();
  void __fastcall Execute();
  virtual void __fastcall DoExecute(TTerminal * Terminal) = 0;
  void __fastcall SetProgress(TFileOperationProgressType & ProgressData);
  void __fastcall GetData(TQueueItemProxy * Proxy);
  virtual bool __fastcall UpdateFileList(TQueueFileList * FileList);
  void __fastcall SetCPSLimit(unsigned long CPSLimit);
  unsigned long __fastcall GetCPSLimit();
  virtual unsigned long __fastcall DefaultCPSLimit();
  virtual UnicodeString __fastcall StartupDirectory() const;
  virtual void __fastcall ProgressUpdated();
  virtual TQueueItem * __fastcall CreateParallelOperation();
  virtual bool __fastcall Complete();
  bool IsExecutionCancelled();
};
//---------------------------------------------------------------------------
class TQueueItemProxy
{
friend class TQueueItem;
friend class TTerminalQueueStatus;
friend class TTerminalQueue;

public:
  bool __fastcall Update();
  bool __fastcall UpdateFileList(TQueueFileList * FileList);
  bool __fastcall ProcessUserAction();
  bool __fastcall Move(bool Sooner);
  bool __fastcall Move(TQueueItemProxy * BeforeItem);
  bool __fastcall ExecuteNow();
  bool __fastcall Delete();
  bool __fastcall Pause();
  bool __fastcall Resume();
  bool __fastcall SetCPSLimit(unsigned long CPSLimit);
  bool __fastcall GetCPSLimit(unsigned long & CPSLimit);

  __property TFileOperationProgressType * ProgressData = { read = GetProgressData };
  __property __int64 TotalTransferred = { read = GetTotalTransferred };
  __property TQueueItem::TInfo * Info = { read = FInfo };
  __property TQueueItem::TStatus Status = { read = FStatus };
  __property bool ProcessingUserAction = { read = FProcessingUserAction };
  __property int Index = { read = GetIndex };
  // Clang warns on property backed by private field which is never used
  void * UserData;

private:
  TFileOperationProgressType * FProgressData;
  TQueueItem::TStatus FStatus;
  TTerminalQueue * FQueue;
  TQueueItem * FQueueItem;
  TTerminalQueueStatus * FQueueStatus;
  TQueueItem::TInfo * FInfo;
  bool FProcessingUserAction;

  __fastcall TQueueItemProxy(TTerminalQueue * Queue, TQueueItem * QueueItem);
  virtual __fastcall ~TQueueItemProxy();
  int __fastcall GetIndex();
  TFileOperationProgressType * __fastcall GetProgressData();
  __int64 __fastcall GetTotalTransferred();
};
//---------------------------------------------------------------------------
class TTerminalQueueStatus
{
friend class TTerminalQueue;
friend class TQueueItemProxy;

public:
  virtual __fastcall ~TTerminalQueueStatus();

  TQueueItemProxy * __fastcall FindByQueueItem(TQueueItem * QueueItem);

  __property int Count = { read = GetCount };
  __property int DoneCount = { read = FDoneCount };
  __property int ActiveCount = { read = GetActiveCount };
  __property int DoneAndActiveCount = { read = GetDoneAndActiveCount };
  __property int ActivePrimaryCount = { read = GetActivePrimaryCount };
  __property int ActiveAndPendingPrimaryCount = { read = GetActiveAndPendingPrimaryCount };
  __property TQueueItemProxy * Items[int Index] = { read = GetItem };

  bool __fastcall IsOnlyOneActiveAndNoPending();

  bool __fastcall UpdateFileList(TQueueItemProxy * ItemProxy, TQueueFileList * FileList);

protected:
  __fastcall TTerminalQueueStatus();

  void __fastcall Add(TQueueItemProxy * ItemProxy);
  void __fastcall Delete(TQueueItemProxy * ItemProxy);
  void __fastcall ResetStats();
  void __fastcall NeedStats();

private:
  TList * FList;
  int FDoneCount;
  int FActiveCount;
  int FActivePrimaryCount;
  int FActiveAndPendingPrimaryCount;

  int __fastcall GetCount();
  int __fastcall GetActiveCount();
  int __fastcall GetDoneAndActiveCount();
  int __fastcall GetActivePrimaryCount();
  int __fastcall GetActiveAndPendingPrimaryCount();
  void __fastcall SetDoneCount(int Value);
  TQueueItemProxy * __fastcall GetItem(int Index);
};
//---------------------------------------------------------------------------
class TBootstrapQueueItem : public TQueueItem
{
public:
  __fastcall TBootstrapQueueItem();

protected:
  virtual void __fastcall DoExecute(TTerminal * Terminal);
  virtual bool __fastcall Complete();
};
//---------------------------------------------------------------------------
class TLocatedQueueItem : public TQueueItem
{
protected:
  __fastcall TLocatedQueueItem(TTerminal * Terminal);
  __fastcall TLocatedQueueItem(const TLocatedQueueItem & Source);

  virtual void __fastcall DoExecute(TTerminal * Terminal);
  virtual UnicodeString __fastcall StartupDirectory() const;

private:
  UnicodeString FCurrentDir;
};
//---------------------------------------------------------------------------
class TTransferQueueItem : public TLocatedQueueItem
{
public:
  __fastcall TTransferQueueItem(TTerminal * Terminal,
    TStrings * FilesToCopy, const UnicodeString & TargetDir,
    const TCopyParamType * CopyParam, int Params, TOperationSide Side,
    bool SingleFile, bool Parallel);
  virtual __fastcall ~TTransferQueueItem();

protected:
  TStrings * FFilesToCopy;
  UnicodeString FTargetDir;
  TCopyParamType * FCopyParam;
  int FParams;
  bool FParallel;
  DWORD FLastParallelOperationAdded;
  std::unique_ptr<TParallelOperation> FParallelOperation;

  virtual unsigned long __fastcall DefaultCPSLimit();
  virtual void __fastcall DoExecute(TTerminal * Terminal);
  virtual void __fastcall DoTransferExecute(TTerminal * Terminal, TParallelOperation * ParallelOperation) = 0;
  virtual void __fastcall ProgressUpdated();
  virtual TQueueItem * __fastcall CreateParallelOperation();
  virtual bool __fastcall UpdateFileList(TQueueFileList * FileList);
};
//---------------------------------------------------------------------------
class TUploadQueueItem : public TTransferQueueItem
{
public:
  __fastcall TUploadQueueItem(TTerminal * Terminal,
    TStrings * FilesToCopy, const UnicodeString & TargetDir,
    const TCopyParamType * CopyParam, int Params, bool Parallel);

protected:
  virtual void __fastcall DoTransferExecute(TTerminal * Terminal, TParallelOperation * ParallelOperation);
};
//---------------------------------------------------------------------------
class TDownloadQueueItem : public TTransferQueueItem
{
public:
  __fastcall TDownloadQueueItem(TTerminal * Terminal,
    TStrings * FilesToCopy, const UnicodeString & TargetDir,
    const TCopyParamType * CopyParam, int Params, bool Parallel);

protected:
  virtual void __fastcall DoTransferExecute(TTerminal * Terminal, TParallelOperation * ParallelOperation);
};
//---------------------------------------------------------------------------
class TRemoteDeleteQueueItem : public TLocatedQueueItem
{
public:
  TRemoteDeleteQueueItem(TTerminal * Terminal, TStrings * FilesToDelete, int Params);

protected:
  virtual void __fastcall DoExecute(TTerminal * Terminal);

private:
  std::unique_ptr<TStrings> FFilesToDelete;
  int FParams;
};
//---------------------------------------------------------------------------
class TLocalDeleteQueueItem : public TQueueItem
{
public:
  TLocalDeleteQueueItem(TStrings * FilesToDelete, int Params);

protected:
  virtual void __fastcall DoExecute(TTerminal * Terminal);

private:
  std::unique_ptr<TStrings> FFilesToDelete;
  int FParams;
};
//---------------------------------------------------------------------------
class TUserAction;
class TTerminalThread : public TSignalThread
{
public:
  __fastcall TTerminalThread(TTerminal * Terminal);
  virtual __fastcall ~TTerminalThread();

  void __fastcall TerminalOpen();
  void __fastcall TerminalReopen();

  void __fastcall Cancel();
  bool __fastcall Release();
  void __fastcall Idle();

  __property TNotifyEvent OnIdle = { read = FOnIdle, write = FOnIdle };
  __property bool Cancelling = { read = FCancel };
  __property bool AllowAbandon = { read = FAllowAbandon, write = FAllowAbandon };


protected:
  virtual void __fastcall ProcessEvent();
  virtual bool __fastcall Finished();

private:
  TTerminal * FTerminal;

  TInformationEvent FOnInformation;
  TQueryUserEvent FOnQueryUser;
  TPromptUserEvent FOnPromptUser;
  TExtendedExceptionEvent FOnShowExtendedException;
  TDisplayBannerEvent FOnDisplayBanner;
  TNotifyEvent FOnChangeDirectory;
  TReadDirectoryEvent FOnReadDirectory;
  TNotifyEvent FOnStartReadDirectory;
  TReadDirectoryProgressEvent FOnReadDirectoryProgress;
  TNotifyEvent FOnInitializeLog;

  TNotifyEvent FOnIdle;

  TNotifyEvent FAction;
  HANDLE FActionEvent;
  TUserAction * FUserAction;

  Exception * FException;
  Exception * FIdleException;
  bool FCancel;
  TDateTime FCancelAfter;
  bool FAbandoned;
  bool FCancelled;
  bool FPendingIdle;
  bool FAllowAbandon;

  DWORD FMainThread;
  TCriticalSection * FSection;

  void __fastcall WaitForUserAction(TUserAction * UserAction);
  void __fastcall RunAction(TNotifyEvent Action);

  static void __fastcall SaveException(Exception & E, Exception *& Exception);
  static void __fastcall Rethrow(Exception *& Exception);
  void __fastcall FatalAbort();
  void __fastcall CheckCancel();

  void __fastcall TerminalOpenEvent(TObject * Sender);
  void __fastcall TerminalReopenEvent(TObject * Sender);

  void __fastcall TerminalInformation(
    TTerminal * Terminal, const UnicodeString & Str, int Phase, const UnicodeString & Additional);
  void __fastcall TerminalQueryUser(TObject * Sender,
    const UnicodeString Query, TStrings * MoreMessages, unsigned int Answers,
    const TQueryParams * Params, unsigned int & Answer, TQueryType Type, void * Arg);
  void __fastcall TerminalPromptUser(TTerminal * Terminal, TPromptKind Kind,
    UnicodeString Name, UnicodeString Instructions,
    TStrings * Prompts, TStrings * Results, bool & Result, void * Arg);
  void __fastcall TerminalShowExtendedException(TTerminal * Terminal,
    Exception * E, void * Arg);
  void __fastcall TerminalDisplayBanner(TTerminal * Terminal,
    UnicodeString SessionName, const UnicodeString & Banner,
    bool & NeverShowAgain, int Options, unsigned int & Params);
  void __fastcall TerminalChangeDirectory(TObject * Sender);
  void __fastcall TerminalReadDirectory(TObject * Sender, Boolean ReloadOnly);
  void __fastcall TerminalStartReadDirectory(TObject * Sender);
  void __fastcall TerminalReadDirectoryProgress(TObject * Sender, int Progress, int ResolvedLinks, bool & Cancel);
  void __fastcall TerminalInitializeLog(TObject * Sender);
  void DiscardException();
};
//---------------------------------------------------------------------------
enum TQueueFileState { qfsQueued = 0, qfsProcessed = 1 };
//---------------------------------------------------------------------------
class TQueueFileList
{
friend class TParallelOperation;
public:
  TQueueFileList();
  void Clear();
  void Add(const UnicodeString & FileName, int State);
  UnicodeString GetFileName(int Index) const;
  int GetState(int Index) const;
  void SetState(int Index, int State);
  int GetCount() const;
private:
  std::unique_ptr<TStrings> FList;
  TParallelOperation * FLastParallelOperation;
  int FLastParallelOperationVersion;
};
//---------------------------------------------------------------------------
#endif
