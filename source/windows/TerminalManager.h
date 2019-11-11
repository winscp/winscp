//---------------------------------------------------------------------------
#ifndef TerminalManagerH
#define TerminalManagerH
//---------------------------------------------------------------------------
#include <Terminal.h>
#include <Queue.h>
#include <FileOperationProgress.h>
#include <WinInterface.h>
#include <Vcl.AppEvnts.hpp>
//---------------------------------------------------------------------------
class TCustomScpExplorerForm;
class TTerminalQueue;
class TAuthenticateForm;
class ITaskbarList3;
//---------------------------------------------------------------------------
enum TTerminalPendingAction { tpNull, tpNone, tpReconnect, tpFree };
//---------------------------------------------------------------------------
class TManagedTerminal : public TTerminal
{
public:
  __fastcall TManagedTerminal(TSessionData * SessionData, TConfiguration * Configuration);
  virtual __fastcall ~TManagedTerminal();

  TSessionData * StateData;
  TObject * LocalExplorerState;
  TObject * RemoteExplorerState;
  TDateTime ReopenStart;
  TDateTime DirectoryLoaded;
  TTerminalThread * TerminalThread;
  TDateTime QueueOperationStart;
};
//---------------------------------------------------------------------------
class TTerminalManager : public TTerminalList
{
public:
  static TTerminalManager * __fastcall Instance(bool ForceCreation = true);
  static void __fastcall DestroyInstance();

  __fastcall TTerminalManager();
  __fastcall ~TTerminalManager();

  virtual TTerminal * __fastcall NewTerminal(TSessionData * Data);
  TTerminal * __fastcall NewTerminals(TList * DataList);
  virtual void __fastcall FreeTerminal(TTerminal * Terminal);
  void __fastcall Move(TTerminal * Source, TTerminal * Target);
  void __fastcall DisconnectActiveTerminal();
  void __fastcall ReconnectActiveTerminal();
  void __fastcall FreeActiveTerminal();
  void __fastcall CycleTerminals(bool Forward);
  bool __fastcall ConnectTerminal(TTerminal * Terminal);
  void __fastcall SetActiveTerminalWithAutoReconnect(TTerminal * value);
  void __fastcall UpdateAppTitle();
  bool __fastcall CanOpenInPutty();
  void __fastcall OpenInPutty();
  void __fastcall NewSession(bool FromSite, const UnicodeString & SessionUrl, bool ReloadSessions = true, TForm * LinkedForm = NULL);
  void __fastcall Idle(bool SkipCurrentTerminal);
  UnicodeString __fastcall GetTerminalShortPath(TTerminal * Terminal);
  UnicodeString __fastcall GetTerminalTitle(TTerminal * Terminal, bool Unique);
  UnicodeString __fastcall GetActiveTerminalTitle(bool Unique);
  UnicodeString __fastcall GetAppProgressTitle();
  void __fastcall HandleException(Exception * E);
  void __fastcall SaveWorkspace(TList * DataList);
  void __fastcall QueueStatusUpdated();
  TTerminal * __fastcall FindActiveTerminalForSite(TSessionData * Data);
  TTerminalQueue * __fastcall FindQueueForTerminal(TTerminal * Terminal);
  bool __fastcall UploadPublicKey(TTerminal * Terminal, TSessionData * Data, UnicodeString & FileName);

  __property TCustomScpExplorerForm * ScpExplorer = { read = FScpExplorer, write = SetScpExplorer };
  __property TTerminal * ActiveTerminal = { read = FActiveTerminal, write = SetActiveTerminal };
  __property TTerminalQueue * ActiveQueue = { read = GetActiveQueue };
  __property int ActiveTerminalIndex = { read = GetActiveTerminalIndex, write = SetActiveTerminalIndex };
  __property TStrings * TerminalList = { read = GetTerminalList };
  __property TNotifyEvent OnLastTerminalClosed = { read = FOnLastTerminalClosed, write = FOnLastTerminalClosed };
  __property TNotifyEvent OnTerminalListChanged = { read = FOnTerminalListChanged, write = FOnTerminalListChanged };
  __property TTerminal * LocalTerminal = { read = FLocalTerminal };

protected:
  virtual TTerminal * __fastcall CreateTerminal(TSessionData * Data);
  void __fastcall DoConnectTerminal(TTerminal * Terminal, bool Reopen, bool AdHoc);

private:
  static TTerminalManager * FInstance;
  TCustomScpExplorerForm * FScpExplorer;
  TTerminal * FActiveTerminal;
  TTerminal * FLocalTerminal;
  bool FDestroying;
  TTerminalPendingAction FTerminalPendingAction;
  TNotifyEvent FOnLastTerminalClosed;
  TNotifyEvent FOnTerminalListChanged;
  TStrings * FTerminalList;
  TList * FQueues;
  TStrings * FTerminationMessages;
  UnicodeString FProgressTitle;
  UnicodeString FForegroundProgressTitle;
  TDateTime FDirectoryReadingStart;
  TAuthenticateForm * FAuthenticateForm;
  TCriticalSection * FQueueSection;
  DWORD FMainThread;
  int FPendingConfigurationChange;
  std::unique_ptr<TCriticalSection> FChangeSection;
  std::vector<std::pair<TTerminalQueue *, TQueueEvent> > FQueueEvents;
  unsigned int FTaskbarButtonCreatedMessage;
  ITaskbarList3 * FTaskbarList;
  int FAuthenticating;
  void * FBusyToken;
  bool FAuthenticationCancelled;
  std::unique_ptr<TApplicationEvents> FApplicationsEvents;
  bool FKeepAuthenticateForm;

  bool __fastcall ConnectActiveTerminalImpl(bool Reopen);
  bool __fastcall ConnectActiveTerminal();
  TTerminalQueue * __fastcall NewQueue(TTerminal * Terminal);
  void __fastcall SetScpExplorer(TCustomScpExplorerForm * value);
  void __fastcall DoSetActiveTerminal(TTerminal * value, bool AutoReconnect);
  void __fastcall SetActiveTerminal(TTerminal * value);
  void __fastcall UpdateAll();
  void __fastcall ApplicationException(TObject * Sender, Exception * E);
  void __fastcall ApplicationShowHint(UnicodeString & HintStr, bool & CanShow,
    THintInfo & HintInfo);
  void __fastcall ApplicationMessage(TMsg & Msg, bool & Handled);
  void __fastcall ConfigurationChange(TObject * Sender);
  void __fastcall TerminalUpdateStatus(TTerminal * Terminal, bool Active);
  void __fastcall TerminalQueryUser(TObject * Sender,
    const UnicodeString Query, TStrings * MoreMessages, unsigned int Answers,
    const TQueryParams * Params, unsigned int & Answer, TQueryType Type, void * Arg);
  void __fastcall TerminalPromptUser(TTerminal * Terminal,
    TPromptKind Kind, UnicodeString Name, UnicodeString Instructions, TStrings * Prompt,
    TStrings * Results, bool & Result, void * Arg);
  void __fastcall TerminalDisplayBanner(TTerminal * Terminal,
    UnicodeString SessionName, const UnicodeString & Banner, bool & NeverShowAgain,
    int Options, unsigned int & Params);
  void __fastcall TerminalShowExtendedException(TTerminal * Terminal,
    Exception * E, void * Arg);
  void __fastcall TerminalReadDirectoryProgress(TObject * Sender, int Progress,
    int ResolvedLinks, bool & Cancel);
  void __fastcall TerminalInformation(TTerminal * Terminal, const UnicodeString & Str,
    bool Status, int Phase);
  void __fastcall TerminalCustomCommand(TTerminal * Terminal, const UnicodeString & Command, bool & Handled);
  void __fastcall FreeAll();
  void __fastcall TerminalReady();
  TStrings * __fastcall GetTerminalList();
  int __fastcall GetActiveTerminalIndex();
  TTerminalQueue * __fastcall GetActiveQueue();
  void __fastcall SaveTerminal(TTerminal * Terminal);
  void __fastcall SetActiveTerminalIndex(int value);
  void __fastcall OperationFinished(::TFileOperation Operation, TOperationSide Side,
    bool Temp, const UnicodeString & FileName, bool Success,
    TOnceDoneOperation & OnceDoneOperation);
  void __fastcall OperationProgress(TFileOperationProgressType & ProgressData);
  void __fastcall DeleteLocalFile(const UnicodeString FileName, bool Alternative, int & Deleted);
  void __fastcall QueueEvent(TTerminalQueue * Queue, TQueueEvent Event);
  TAuthenticateForm * __fastcall MakeAuthenticateForm(TTerminal * Terminal);
  void __fastcall MasterPasswordPrompt();
  void __fastcall FileNameInputDialogInitializeRenameBaseName(
    TObject * Sender, TInputDialogData * Data);
  void __fastcall InitTaskbarButtonCreatedMessage();
  void __fastcall ReleaseTaskbarList();
  void __fastcall CreateTaskbarList();
  void __fastcall UpdateTaskbarList();
  void __fastcall AuthenticateFormCancel(TObject * Sender);
  void __fastcall DoTerminalListChanged();
  TTerminal * __fastcall DoNewTerminal(TSessionData * Data);
  static void __fastcall TerminalThreadIdle(void * Data, TObject * Sender);
  void __fastcall SetQueueConfiguration(TTerminalQueue * Queue);
  void __fastcall ApplicationModalBegin(TObject * Sender);
  void __fastcall ApplicationModalEnd(TObject * Sender);
  bool __fastcall HandleMouseWheel(WPARAM WParam, LPARAM LParam);
  void __fastcall DoConfigurationChange();
  bool __fastcall ShouldDisplayQueueStatusOnAppTitle();
  void __fastcall SetupTerminal(TTerminal * Terminal);
  void __fastcall CloseAutheticateForm();
  void __fastcall AuthenticatingDone();
  TRemoteFile * __fastcall CheckRights(
    TTerminal * Terminal, const UnicodeString & EntryType, const UnicodeString & FileName, bool & WrongRights);
};
//---------------------------------------------------------------------------
#endif
