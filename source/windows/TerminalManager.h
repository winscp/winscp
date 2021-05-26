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

  bool LocalBrowser;
  TSessionData * StateData;
  TObject * LocalExplorerState;
  TObject * RemoteExplorerState;
  TObject * OtherLocalExplorerState;
  TDateTime ReopenStart;
  TDateTime DirectoryLoaded;
  TTerminalThread * TerminalThread;
  TDateTime QueueOperationStart;
  // To distinguish sessions that were explicitly disconnected and
  // should not be reconnected when their tab is activated.
  bool Disconnected;
  bool DisconnectedTemporarily;
  // Sessions that should not close when they fail to connect
  // (i.e. those that were ever connected or were opened as a part of a workspace)
  bool Permanent;
};
//---------------------------------------------------------------------------
class TTerminalManager : public TTerminalList
{
public:
  static TTerminalManager * __fastcall Instance(bool ForceCreation = true);
  static void __fastcall DestroyInstance();

  __fastcall TTerminalManager();
  __fastcall ~TTerminalManager();

  TManagedTerminal * __fastcall NewManagedTerminal(TSessionData * Data);
  TManagedTerminal * __fastcall NewLocalBrowser(
    const UnicodeString & LocalDirectory = UnicodeString(), const UnicodeString & OtherLocalDirectory = UnicodeString());
  TManagedTerminal * __fastcall NewSessions(TList * DataList);
  virtual void __fastcall FreeTerminal(TTerminal * Terminal);
  void __fastcall Move(TTerminal * Source, TTerminal * Target);
  void __fastcall DisconnectActiveTerminalIfPermanentFreeOtherwise();
  void __fastcall DisconnectActiveTerminal();
  void __fastcall ReconnectActiveTerminal();
  void __fastcall FreeActiveTerminal();
  void __fastcall CycleTerminals(bool Forward);
  bool __fastcall ConnectTerminal(TTerminal * Terminal);
  void __fastcall SetActiveTerminalWithAutoReconnect(TManagedTerminal * value);
  void __fastcall UpdateAppTitle();
  bool __fastcall CanOpenInPutty();
  void __fastcall OpenInPutty();
  void __fastcall NewSession(
    const UnicodeString & SessionUrl, bool ReloadSessions = true, TForm * LinkedForm = NULL, bool ReplaceExisting = false);
  void NewLocalSession(const UnicodeString & LocalDirectory = UnicodeString(), const UnicodeString & OtherLocalDirectory = UnicodeString());
  void __fastcall Idle(bool SkipCurrentTerminal);
  UnicodeString __fastcall GetSessionTitle(TManagedTerminal * Terminal, bool Unique);
  UnicodeString __fastcall GetActiveSessionAppTitle();
  UnicodeString __fastcall GetAppProgressTitle();
  UnicodeString __fastcall FormatFormCaptionWithSession(TCustomForm * Form, const UnicodeString & Caption);
  void __fastcall HandleException(Exception * E);
  void __fastcall SaveWorkspace(TList * DataList);
  void __fastcall QueueStatusUpdated();
  bool __fastcall IsActiveTerminalForSite(TTerminal * Terminal, TSessionData * Data);
  TTerminal * __fastcall FindActiveTerminalForSite(TSessionData * Data);
  TTerminalQueue * __fastcall FindQueueForTerminal(TTerminal * Terminal);
  bool __fastcall UploadPublicKey(TTerminal * Terminal, TSessionData * Data, UnicodeString & FileName);
  UnicodeString GetPathForSessionTabName(const UnicodeString & Result);

  __property TCustomScpExplorerForm * ScpExplorer = { read = FScpExplorer, write = SetScpExplorer };
  __property TManagedTerminal * ActiveSession = { read = FActiveSession, write = SetActiveSession };
  __property TManagedTerminal * ActiveTerminal = { read = GetActiveTerminal };
  __property TTerminalQueue * ActiveQueue = { read = GetActiveQueue };
  __property int ActiveSessionIndex = { read = GetActiveSessionIndex, write = SetActiveSessionIndex };
  __property TStrings * SessionList = { read = GetSessionList };
  __property TTerminal * LocalTerminal = { read = FLocalTerminal };
  __property TManagedTerminal * Sessions[int Index]  = { read = GetSession };
  __property bool Updating = { read = IsUpdating };

protected:
  virtual TTerminal * __fastcall CreateTerminal(TSessionData * Data);
  void __fastcall DoConnectTerminal(TTerminal * Terminal, bool Reopen, bool AdHoc);
  virtual TTerminal * __fastcall NewTerminal(TSessionData * Data);

private:
  static TTerminalManager * FInstance;
  TCustomScpExplorerForm * FScpExplorer;
  TManagedTerminal * FActiveSession;
  TTerminal * FLocalTerminal;
  bool FDestroying;
  TTerminalPendingAction FTerminalPendingAction;
  TStrings * FSessionList;
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
  int FUpdating;

  bool __fastcall ConnectActiveTerminalImpl(bool Reopen);
  bool __fastcall ConnectActiveTerminal();
  TTerminalQueue * __fastcall NewQueue(TTerminal * Terminal);
  void __fastcall SetScpExplorer(TCustomScpExplorerForm * value);
  void UpdateScpExplorer();
  void UpdateScpExplorer(TManagedTerminal * Session, TTerminalQueue * Queue);
  void __fastcall DoSetActiveSession(TManagedTerminal * value, bool AutoReconnect, bool LastTerminalClosed);
  void __fastcall SetActiveSession(TManagedTerminal * value);
  TManagedTerminal * GetActiveTerminal();
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
  void __fastcall TerminalInformation(
    TTerminal * Terminal, const UnicodeString & Str, bool Status, int Phase, const UnicodeString & Additional);
  void __fastcall TerminalCustomCommand(TTerminal * Terminal, const UnicodeString & Command, bool & Handled);
  void __fastcall FreeAll();
  void __fastcall SessionReady();
  TStrings * __fastcall GetSessionList();
  int __fastcall GetActiveSessionIndex();
  TTerminalQueue * __fastcall GetActiveQueue();
  void __fastcall SaveTerminal(TTerminal * Terminal);
  void __fastcall SetActiveSessionIndex(int value);
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
  void __fastcall DoSessionListChanged();
  TManagedTerminal * __fastcall DoNewSession(TSessionData * Data);
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
  TManagedTerminal * __fastcall CreateManagedTerminal(TSessionData * Data);
  TManagedTerminal * __fastcall GetSession(int Index);
  bool IsUpdating();
};
//---------------------------------------------------------------------------
#endif
