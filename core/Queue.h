//---------------------------------------------------------------------------
#ifndef QueueH
#define QueueH
//---------------------------------------------------------------------------
class TSignalThread
{
friend int __fastcall ThreadProc(void * Thread);

public:
  void __fastcall Start();
  void __fastcall Terminate();
  void __fastcall WaitFor();
  void __fastcall TriggerEvent();

protected:
  HANDLE FThread;
  HANDLE FEvent;
  bool FTerminated;
  bool FFinished;

  __fastcall TSignalThread();
  virtual __fastcall ~TSignalThread();

  bool __fastcall WaitForEvent();
  virtual void __fastcall Execute();
  virtual void __fastcall Finished();
  virtual void __fastcall ProcessEvent() = 0;
  void __fastcall Close();
};
//---------------------------------------------------------------------------
class TTerminal;
class TQueueItem;
class TCriticalSection;
class TTerminalQueue;
class TQueueItemProxy;
class TTerminalQueueStatus;
//---------------------------------------------------------------------------
typedef void __fastcall (__closure * TQueueListUpdate)
  (TTerminalQueue * Queue);
typedef void __fastcall (__closure * TQueueItemUpdateEvent)
  (TTerminalQueue * Queue, TQueueItem * Item);
//---------------------------------------------------------------------------
class TTerminalQueue : public TSignalThread
{
friend class TTerminalItem;
friend class TQueueItem;
friend class TQueueItemProxy;

public:
  __fastcall TTerminalQueue(TTerminal * Terminal, TConfiguration * Configuration);
  virtual __fastcall ~TTerminalQueue();

  void __fastcall AddItem(TQueueItem * Item);
  TTerminalQueueStatus * __fastcall CreateStatus(TTerminalQueueStatus * Current);

  __property bool IsEmpty = { read = GetIsEmpty };
  __property int TransfersLimit = { read = FTransfersLimit, write = SetTransfersLimit };
  __property TQueryUserEvent OnQueryUser = { read = FOnQueryUser, write = FOnQueryUser };
  __property TPromptUserEvent OnPromptUser = { read = FOnPromptUser, write = FOnPromptUser };
  __property TExtendedExceptionEvent OnShowExtendedException = { read = FOnShowExtendedException, write = FOnShowExtendedException };
  __property TQueueListUpdate OnListUpdate = { read = FOnListUpdate, write = FOnListUpdate };
  __property TQueueItemUpdateEvent OnQueueItemUpdate = { read = FOnQueueItemUpdate, write = FOnQueueItemUpdate };

protected:
  TQueryUserEvent FOnQueryUser;
  TPromptUserEvent FOnPromptUser;
  TExtendedExceptionEvent FOnShowExtendedException;
  TQueueItemUpdateEvent FOnQueueItemUpdate;
  TQueueListUpdate FOnListUpdate;
  TTerminal * FTerminal;
  TConfiguration * FConfiguration;
  TSessionData * FSessionData;
  TList * FItems;
  int FItemsInProcess;
  TCriticalSection * FItemsSection;
  int FFreeTerminals;
  TList * FTerminals;
  int FTemporaryTerminals;
  int FTransfersLimit;

  TQueueItem * __fastcall GetItem(int Index);
  bool __fastcall ItemGetData(TQueueItem * Item, TQueueItemProxy * Proxy);
  bool __fastcall ItemProcessUserAction(TQueueItem * Item);
  bool __fastcall ItemMove(TQueueItem * Item, TQueueItem * BeforeItem);
  bool __fastcall ItemExecuteNow(TQueueItem * Item);
  bool __fastcall ItemDelete(TQueueItem * Item);

  void __fastcall RetryItem(TQueueItem * Item);
  void __fastcall DeleteItem(TQueueItem * Item);

  virtual void __fastcall ProcessEvent();
  void __fastcall TerminalFinished(TTerminalItem * TerminalItem);
  bool __fastcall TerminalFree(TTerminalItem * TerminalItem);

  void __fastcall DoQueryUser(TObject * Sender, const AnsiString Query,
    TStrings * MoreMessages, int Answers, int Params, int & Answer,
    TQueryType Type);
  void __fastcall DoPromptUser(TSecureShell * SecureShell, AnsiString Prompt,
    TPromptKind Kind, AnsiString & Response, bool & Result);
  void __fastcall DoShowExtendedException(TSecureShell * SecureShell,
    Exception * E);
  void __fastcall DoQueueItemUpdate(TQueueItem * Item);
  void __fastcall DoListUpdate();

  void __fastcall SetTransfersLimit(int value);
  bool __fastcall GetIsEmpty();
};
//---------------------------------------------------------------------------
class TQueueItem
{
friend class TTerminalQueue;
friend class TTerminalItem;

public:
  enum TStatus {
    qsPending, qsConnecting, qsProcessing, qsPrompt, qsQuery, qsError, qsDone };
  struct TInfo
  {
    TFileOperation Operation;
    TOperationSide Side;
    AnsiString Source;
    AnsiString Destination;
    bool ModifiesLocal;
    bool ModifiesRemote;
  };

  static bool __fastcall IsUserActionStatus(TStatus Status);

  __property TStatus Status = { read = GetStatus };

protected:
  TStatus FStatus;
  TCriticalSection * FSection;
  TTerminalItem * FTerminalItem;
  TFileOperationProgressType * FProgressData;
  TQueueItem::TInfo * FInfo;
  TTerminalQueue * FQueue;

  __fastcall TQueueItem();
  virtual __fastcall ~TQueueItem();

  void __fastcall SetStatus(TStatus Status);
  TStatus __fastcall GetStatus();
  void __fastcall Execute(TTerminalItem * TerminalItem);
  virtual void __fastcall DoExecute(TTerminal * Terminal) = 0;
  void __fastcall SetProgress(const TFileOperationProgressType & ProgressData);
  void __fastcall GetData(TQueueItemProxy * Proxy);
};
//---------------------------------------------------------------------------
class TQueueItemProxy
{
friend class TQueueItem;
friend class TTerminalQueueStatus;
friend class TTerminalQueue;

public:
  bool __fastcall Update();
  bool __fastcall ProcessUserAction();
  bool __fastcall Move(bool Sooner);
  bool __fastcall Move(TQueueItemProxy * BeforeItem);
  bool __fastcall ExecuteNow();
  bool __fastcall Delete();

  __property TFileOperationProgressType * ProgressData = { read = GetProgressData };
  __property TQueueItem::TInfo * Info = { read = FInfo };
  __property TQueueItem::TStatus Status = { read = FStatus };
  __property bool Flag = { read = FFlag, write = FFlag };
  __property bool ProcessingUserAction = { read = FProcessingUserAction };
  __property int Index = { read = GetIndex };

private:
  TFileOperationProgressType * FProgressData;
  TQueueItem::TStatus FStatus;
  TTerminalQueue * FQueue;
  TQueueItem * FQueueItem;
  TTerminalQueueStatus * FQueueStatus;
  TQueueItem::TInfo * FInfo;
  bool FFlag;
  bool FProcessingUserAction;

  __fastcall TQueueItemProxy(TTerminalQueue * Queue, TQueueItem * QueueItem);
  virtual __fastcall ~TQueueItemProxy();
  int __fastcall GetIndex();
  TFileOperationProgressType * __fastcall GetProgressData();
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
  __property int ActiveCount = { read = GetActiveCount };
  __property TQueueItemProxy * Items[int Index] = { read = GetItem };

protected:
  __fastcall TTerminalQueueStatus();

  void __fastcall Add(TQueueItemProxy * ItemProxy);
  void __fastcall Delete(TQueueItemProxy * ItemProxy);
  void __fastcall ResetStats();

private:
  TList * FList;
  int FActiveCount;

  int __fastcall GetCount();
  int __fastcall GetActiveCount();
  TQueueItemProxy * __fastcall GetItem(int Index);
};
//---------------------------------------------------------------------------
class TLocatedQueueItem : public TQueueItem
{
protected:
  __fastcall TLocatedQueueItem(TTerminal * Terminal);

  virtual void __fastcall DoExecute(TTerminal * Terminal);

private:
  AnsiString FCurrentDir;
};
//---------------------------------------------------------------------------
class TTransferQueueItem : public TLocatedQueueItem
{
public:
  __fastcall TTransferQueueItem(TTerminal * Terminal,
    TStrings * FilesToCopy, const AnsiString & TargetDir,
    const TCopyParamType * CopyParam, int Params, TOperationSide Side);
  virtual __fastcall ~TTransferQueueItem();

protected:
  TStrings * FFilesToCopy;
  AnsiString FTargetDir;
  TCopyParamType * FCopyParam;
  int FParams;
};
//---------------------------------------------------------------------------
class TUploadQueueItem : public TTransferQueueItem
{
public:
  __fastcall TUploadQueueItem(TTerminal * Terminal,
    TStrings * FilesToCopy, const AnsiString & TargetDir,
    const TCopyParamType * CopyParam, int Params);

protected:
  virtual void __fastcall DoExecute(TTerminal * Terminal);
};
//---------------------------------------------------------------------------
class TDownloadQueueItem : public TTransferQueueItem
{
public:
  __fastcall TDownloadQueueItem(TTerminal * Terminal,
    TStrings * FilesToCopy, const AnsiString & TargetDir,
    const TCopyParamType * CopyParam, int Params);

protected:
  virtual void __fastcall DoExecute(TTerminal * Terminal);
};
//---------------------------------------------------------------------------
#endif
